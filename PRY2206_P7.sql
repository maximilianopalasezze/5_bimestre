/* ============================================================
   SEMANA 7 - CLINICA MAXSALUD
   Objetos a crear:
   1) Package PKG_MOROSIDAD_MAXSALUD (spec + body)
   2) Función FN_NOMBRE_ESPECIALIDAD
   3) Procedimiento SP_GENERAR_PAGO_MOROSO (año anterior, VARRAY, IF/ELSIF)
   4) Triggers (auditoría/bloqueo para evitar intervención manual)
   ============================================================ */

---------------------------------------------------------------
-- 0) TABLA DE AUDITORÍA (para triggers)
---------------------------------------------------------------
BEGIN
  EXECUTE IMMEDIATE '
    CREATE TABLE AUD_PAGO_MOROSO (
      id_aud        NUMBER GENERATED ALWAYS AS IDENTITY,
      fecha_evento  DATE DEFAULT SYSDATE NOT NULL,
      usuario_db    VARCHAR2(30) DEFAULT USER NOT NULL,
      evento        VARCHAR2(10) NOT NULL,
      ate_id        NUMBER,
      detalle       VARCHAR2(200)
    )';
EXCEPTION
  WHEN OTHERS THEN
    /* Si ya existe, no hacemos nada */
    IF SQLCODE != -955 THEN
      RAISE;
    END IF;
END;
/
---------------------------------------------------------------
-- 1) PACKAGE (SPEC)
---------------------------------------------------------------
CREATE OR REPLACE PACKAGE PKG_MOROSIDAD_MAXSALUD AS
  /* Variable pública: multa base (sin descuento) */
  g_multa_base      NUMBER := 0;

  /* Variable pública: descuento aplicado en $ (si corresponde) */
  g_descuento_multa NUMBER := 0;

  /* Función pública: retorna % descuento para pacientes > 70 */
  FUNCTION fn_pct_descto_3ra_edad(p_edad IN NUMBER) RETURN NUMBER;
END PKG_MOROSIDAD_MAXSALUD;
/
SHOW ERRORS;

---------------------------------------------------------------
-- 1) PACKAGE (BODY)
---------------------------------------------------------------
CREATE OR REPLACE PACKAGE BODY PKG_MOROSIDAD_MAXSALUD AS

  FUNCTION fn_pct_descto_3ra_edad(p_edad IN NUMBER) RETURN NUMBER IS
    v_pct NUMBER := 0;
  BEGIN
    /* Busca el % de descuento según rango de edad */
    SELECT porcentaje_descto
      INTO v_pct
      FROM PORC_DESCTO_3RA_EDAD
     WHERE p_edad BETWEEN anno_ini AND anno_ter;

    RETURN NVL(v_pct, 0);

  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      RETURN 0;
    WHEN OTHERS THEN
      RETURN 0;
  END fn_pct_descto_3ra_edad;

END PKG_MOROSIDAD_MAXSALUD;
/
SHOW ERRORS;

---------------------------------------------------------------
-- 2) FUNCIÓN: nombre especialidad por atención
---------------------------------------------------------------
CREATE OR REPLACE FUNCTION FN_NOMBRE_ESPECIALIDAD(p_ate_id IN NUMBER)
RETURN VARCHAR2
IS
  v_nombre ESPECIALIDAD.nombre%TYPE;
BEGIN
  SELECT e.nombre
    INTO v_nombre
    FROM ATENCION a
    JOIN MEDICO m       ON m.med_run = a.med_run
    JOIN ESPECIALIDAD e ON e.esp_id  = m.esp_id
   WHERE a.ate_id = p_ate_id;

  RETURN v_nombre;

EXCEPTION
  WHEN NO_DATA_FOUND THEN
    RETURN 'SIN ESPECIALIDAD';
  WHEN OTHERS THEN
    RETURN 'SIN ESPECIALIDAD';
END;
/
SHOW ERRORS;

---------------------------------------------------------------
-- 3) PROCEDIMIENTO PRINCIPAL
---------------------------------------------------------------
CREATE OR REPLACE PROCEDURE SP_GENERAR_PAGO_MOROSO
IS
  /*=========================
    VARRAY: multas por día (Tabla 1)
    1 Medicina General            1200
    2 Traumatologia              1300
    3 Neurologia / Pediatria     1700
    4 Oftalmologia               1900
    5 Geriatria                  1100
    6 Ginecologia / Gastro       2000
    7 Dermatologia               2300
  =========================*/
  TYPE t_varray_multas IS VARRAY(7) OF NUMBER;
  v_multas t_varray_multas := t_varray_multas(1200,1300,1700,1900,1100,2000,2300);

  /* Año anterior (dinámico) */
  v_ini_anio DATE := TRUNC(ADD_MONTHS(SYSDATE, -12), 'YYYY');
  v_fin_anio DATE := TRUNC(SYSDATE, 'YYYY') - 1;

  /* Variables de cálculo */
  v_especialidad    VARCHAR2(50);
  v_multa_dia       NUMBER := 0;
  v_dias_morosidad  NUMBER := 0;
  v_edad            NUMBER := 0;
  v_pct_descto      NUMBER := 0;
  v_obs             VARCHAR2(200);

  /* Manejo de error */
  v_corr NUMBER := 0;
  v_err  VARCHAR2(500);  -- ✅ ESTA ES LA QUE TE FALTABA

  /* Cursor: atenciones pagadas fuera de plazo en el año anterior */
  CURSOR c_morosos IS
    SELECT  p.pac_run,
            p.dv_run,
            (p.pnombre || ' ' || p.snombre || ' ' || p.apaterno || ' ' || p.amaterno) AS nombre_completo,
            p.apaterno,
            a.ate_id,
            a.fecha_atencion,
            p.fecha_nacimiento,
            pa.fecha_venc_pago,
            pa.fecha_pago,
            a.costo
      FROM PACIENTE p
      JOIN ATENCION a        ON a.pac_run = p.pac_run
      JOIN PAGO_ATENCION pa  ON pa.ate_id = a.ate_id
     WHERE pa.fecha_pago > pa.fecha_venc_pago
       AND pa.fecha_pago BETWEEN v_ini_anio AND v_fin_anio
     ORDER BY pa.fecha_venc_pago ASC, p.apaterno ASC;

BEGIN
  /* Truncar tablas en tiempo de ejecución */
  EXECUTE IMMEDIATE 'TRUNCATE TABLE PAGO_MOROSO';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE ERRORES_PROCESO';

  /* Proceso */
  FOR r IN c_morosos LOOP

    /* 1) Especialidad usando función almacenada */
    v_especialidad := FN_NOMBRE_ESPECIALIDAD(r.ate_id);

    /* 2) Días de morosidad */
    v_dias_morosidad := TRUNC(r.fecha_pago - r.fecha_venc_pago);

    /* 3) Multa por día según especialidad (IF/ELSIF obligatorio) */
    v_multa_dia := 0;

    IF UPPER(v_especialidad) = UPPER('Medicina General') THEN
      v_multa_dia := v_multas(1);

    ELSIF UPPER(v_especialidad) = UPPER('Traumatologia') THEN
      v_multa_dia := v_multas(2);

    ELSIF UPPER(v_especialidad) = UPPER('Neurologia')
       OR UPPER(v_especialidad) = UPPER('Pediatria') THEN
      v_multa_dia := v_multas(3);

    ELSIF UPPER(v_especialidad) = UPPER('Oftalmologia') THEN
      v_multa_dia := v_multas(4);

    ELSIF UPPER(v_especialidad) = UPPER('Geriatria') THEN
      v_multa_dia := v_multas(5);

    ELSIF UPPER(v_especialidad) = UPPER('Ginecologia')
       OR UPPER(v_especialidad) = UPPER('Gastroenterologia') THEN
      v_multa_dia := v_multas(6);

    ELSIF UPPER(v_especialidad) = UPPER('Dermatologia') THEN
      v_multa_dia := v_multas(7);

    ELSE
      v_multa_dia := 0;
    END IF;

    /* 4) Multa base (guardar en variable pública del package) */
    PKG_MOROSIDAD_MAXSALUD.g_multa_base := v_dias_morosidad * v_multa_dia;

    /* 5) Edad a la fecha de atención */
    v_edad := TRUNC(MONTHS_BETWEEN(r.fecha_atencion, r.fecha_nacimiento) / 12);

    /* 6) Descuento > 70 (usando package) */
    PKG_MOROSIDAD_MAXSALUD.g_descuento_multa := 0;
    v_obs := NULL;

    IF v_edad > 70 THEN
      v_pct_descto := PKG_MOROSIDAD_MAXSALUD.fn_pct_descto_3ra_edad(v_edad);

      PKG_MOROSIDAD_MAXSALUD.g_descuento_multa :=
        TRUNC(PKG_MOROSIDAD_MAXSALUD.g_multa_base * (v_pct_descto / 100));

      v_obs := 'Paciente tenía ' || v_edad ||
               ' a la fecha de atención. Se aplicó descuento paciente mayor a 70 años';
    ELSE
      v_pct_descto := 0;
      PKG_MOROSIDAD_MAXSALUD.g_descuento_multa := 0;
      v_obs := NULL;
    END IF;

    /* 7) Insert en PAGO_MOROSO */
    INSERT INTO PAGO_MOROSO
      (pac_run, pac_dv_run, pac_nombre, ate_id,
       fecha_venc_pago, fecha_pago, dias_morosidad,
       especialidad_atencion, costo_atencion, monto_multa, observacion)
    VALUES
      (r.pac_run, r.dv_run, r.nombre_completo, r.ate_id,
       r.fecha_venc_pago, r.fecha_pago, v_dias_morosidad,
       v_especialidad, r.costo,
       (PKG_MOROSIDAD_MAXSALUD.g_multa_base - PKG_MOROSIDAD_MAXSALUD.g_descuento_multa),
       v_obs);

  END LOOP;

  COMMIT;

EXCEPTION
  WHEN OTHERS THEN
    /* Correlativo */
    SELECT NVL(MAX(NRO_CORRELATIVO),0) + 1
      INTO v_corr
      FROM ERRORES_PROCESO;

    /* Mensaje de error seguro */
    v_err := SUBSTR(DBMS_UTILITY.FORMAT_ERROR_STACK, 1, 500);

    /* Insert seguro (tu tabla tiene 3 columnas) */
    INSERT INTO ERRORES_PROCESO
    VALUES (v_corr, 'SP_GENERAR_PAGO_MOROSO', v_err);

    ROLLBACK;
END SP_GENERAR_PAGO_MOROSO;
/
SHOW ERRORS;

---------------------------------------------------------------
-- 4) TRIGGERS (tema semana: auditoría + evitar intervención)
---------------------------------------------------------------

/* Trigger 1: audita inserciones en PAGO_MOROSO */
CREATE OR REPLACE TRIGGER TRG_AUD_INS_PAGO_MOROSO
AFTER INSERT ON PAGO_MOROSO
FOR EACH ROW
BEGIN
  INSERT INTO AUD_PAGO_MOROSO(evento, ate_id, detalle)
  VALUES (
    'INSERT',
    :NEW.ate_id,
    'Se registró morosidad para RUT ' || :NEW.pac_run || '-' || :NEW.pac_dv_run
  );
END;
/
SHOW ERRORS;

/* Trigger 2: bloquea (y audita) modificación manual de MONTO_MULTA */
CREATE OR REPLACE TRIGGER TRG_BLOQ_UPD_MULTA_PAGO_MOROSO
BEFORE UPDATE OF MONTO_MULTA ON PAGO_MOROSO
FOR EACH ROW
BEGIN
  INSERT INTO AUD_PAGO_MOROSO(evento, ate_id, detalle)
  VALUES (
    'UPDATE',
    :OLD.ate_id,
    'Intento cambio multa: ' || :OLD.monto_multa || ' -> ' || :NEW.monto_multa
  );

  RAISE_APPLICATION_ERROR(-20010, 'No se permite modificar manualmente MONTO_MULTA en PAGO_MOROSO.');
END;
/
SHOW ERRORS;

---------------------------------------------------------------
-- 5) EJECUCIÓN DEL PROCESO
---------------------------------------------------------------
BEGIN
  SP_GENERAR_PAGO_MOROSO;
END;
/

SELECT 'Medicina General' AS especialidad, 1200 AS multa_por_dia_atraso FROM dual UNION ALL
SELECT 'Traumatología', 1300 FROM dual UNION ALL
SELECT 'Neurología y Pediatría', 1700 FROM dual UNION ALL
SELECT 'Oftalmología', 1900 FROM dual UNION ALL
SELECT 'Geriatría', 1100 FROM dual UNION ALL
SELECT 'Ginecología y Gastroenterología', 2000 FROM dual UNION ALL
SELECT 'Dermatología', 2300 FROM dual
ORDER BY especialidad;

SELECT
  pm.pac_run, pm.pac_dv_run, pm.pac_nombre,
  pm.ate_id, pm.fecha_venc_pago, pm.fecha_pago,
  pm.dias_morosidad, pm.especialidad_atencion,
  pm.costo_atencion, pm.monto_multa
FROM pago_moroso pm
JOIN paciente p ON p.pac_run = pm.pac_run
ORDER BY pm.fecha_venc_pago ASC, p.apaterno ASC;

SELECT pm.observacion
FROM pago_moroso pm
JOIN paciente p ON p.pac_run = pm.pac_run
ORDER BY pm.fecha_venc_pago ASC, p.apaterno ASC;