/*===============================================================
  SEMANA 3 
  CLÍNICA KETEKURA
  ---------------------------------------------------------------
  CASO 1: PAGO_MOROSO (Año paramétrico con BIND)
  CASO 2: MEDICO_SERVICIO_COMUNIDAD (Año anterior automático)

===============================================================*/

--===============================================================
-- 0) FORMATO DE SALIDA 
--===============================================================
SET LINESIZE 200
SET PAGESIZE 100
SET SERVEROUTPUT ON

COLUMN unidad               FORMAT A28
COLUMN run_medico           FORMAT A15
COLUMN nombre_medico        FORMAT A35
COLUMN correo_institucional FORMAT A28
COLUMN destinacion          FORMAT A45

COLUMN pac_nombre            FORMAT A35
COLUMN especialidad_atencion FORMAT A30

--===============================================================
-- 1) CASO 1 - PAGO_MOROSO
--    Requisitos: Cursor explícito + TRUNCATE + VARRAY + RECORD +
--                BIND + IF/ELSIF + almacenamiento en PAGO_MOROSO
--===============================================================

/*----------------------------------------------------------------
  1.1) BIND VARIABLE 
  Si acreditación = 2026 => reporta 2025
----------------------------------------------------------------*/
VARIABLE b_anno_acreditacion NUMBER;

EXEC :b_anno_acreditacion := EXTRACT(YEAR FROM SYSDATE);


PRINT b_anno_acreditacion;

DECLARE
  -- Año ingresado paramétricamente 
  v_anno_acreditacion NUMBER := :b_anno_acreditacion;

  -- Año a reportar = año anterior
  v_anno_reporte NUMBER := v_anno_acreditacion - 1;

  -- Rango dinámico del año a reportar (sin fechas fijas)
  v_fec_ini DATE := TO_DATE('01-01-' || TO_CHAR(v_anno_reporte), 'DD-MM-YYYY');
  v_fec_fin DATE := ADD_MONTHS(v_fec_ini, 12) - 1;

  -- VARRAY multas por día 
  TYPE t_varray_multas IS VARRAY(7) OF NUMBER;
  v_multas t_varray_multas := t_varray_multas(1200,1300,1700,1900,1100,2000,2300);

  TYPE r_moroso IS RECORD (
    pac_run      paciente.pac_run%TYPE,
    pac_dv       paciente.dv_run%TYPE,
    pac_nombre   VARCHAR2(60),
    ate_id       atencion.ate_id%TYPE,
    fec_venc     pago_atencion.fecha_venc_pago%TYPE,
    fec_pago     pago_atencion.fecha_pago%TYPE,
    dias_mora    NUMBER,
    espec_nombre especialidad.nombre%TYPE,
    edad         NUMBER,
    pct_desc     porc_descto_3ra_edad.porcentaje_descto%TYPE,
    multa_dia    NUMBER,
    monto_multa  pago_moroso.monto_multa%TYPE
  );

  v_reg r_moroso;

  -- Fecha nacimiento con %TYPE
  v_fecha_nac paciente.fecha_nacimiento%TYPE;

  CURSOR c_morosidad IS
    SELECT
      p.pac_run,
      p.dv_run,
      (p.pnombre || ' ' || p.snombre || ' ' || p.apaterno || ' ' || p.amaterno) AS nombre_completo,
      pa.ate_id,
      pa.fecha_venc_pago,
      pa.fecha_pago,
      e.nombre AS especialidad,
      p.fecha_nacimiento
    FROM pago_atencion pa
      JOIN atencion a     ON a.ate_id = pa.ate_id
      JOIN paciente p     ON p.pac_run = a.pac_run
      JOIN especialidad e ON e.esp_id = a.esp_id
    WHERE pa.fecha_pago IS NOT NULL
      AND pa.fecha_pago > pa.fecha_venc_pago
      AND pa.fecha_pago BETWEEN v_fec_ini AND v_fec_fin
    ORDER BY pa.fecha_venc_pago ASC, p.apaterno ASC;

BEGIN
  -- Debug opcional del rango real
  DBMS_OUTPUT.PUT_LINE('CASO 1 -> AÑO ACREDITACIÓN = ' || v_anno_acreditacion);
  DBMS_OUTPUT.PUT_LINE('CASO 1 -> AÑO REPORTE     = ' || v_anno_reporte);
  DBMS_OUTPUT.PUT_LINE('CASO 1 -> FECHA INI       = ' || TO_CHAR(v_fec_ini, 'DD-MM-YYYY'));
  DBMS_OUTPUT.PUT_LINE('CASO 1 -> FECHA FIN       = ' || TO_CHAR(v_fec_fin, 'DD-MM-YYYY'));

  -- TRUNCATE en tiempo de ejecución (permite re-ejecutar el bloque)
  EXECUTE IMMEDIATE 'TRUNCATE TABLE PAGO_MOROSO';

  OPEN c_morosidad;
  LOOP
    FETCH c_morosidad INTO
      v_reg.pac_run,
      v_reg.pac_dv,
      v_reg.pac_nombre,
      v_reg.ate_id,
      v_reg.fec_venc,
      v_reg.fec_pago,
      v_reg.espec_nombre,
      v_fecha_nac;

    EXIT WHEN c_morosidad%NOTFOUND;

    -- Días de morosidad (pago fuera de plazo)
    v_reg.dias_mora := TRUNC(v_reg.fec_pago) - TRUNC(v_reg.fec_venc);

    -- Edad dinámica (sin fechas fijas)
    v_reg.edad := TRUNC(MONTHS_BETWEEN(SYSDATE, v_fecha_nac) / 12);

    -- % descuento tercera edad (si no existe registro, 0)
    BEGIN
      SELECT porcentaje_descto
        INTO v_reg.pct_desc
        FROM porc_descto_3ra_edad
       WHERE v_reg.edad BETWEEN anno_ini AND anno_ter;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_reg.pct_desc := 0;
    END;

    -- Multa por día según especialidad 
    IF v_reg.espec_nombre IN ('Cirugía General', 'Dermatología') THEN
      v_reg.multa_dia := v_multas(1);
    ELSIF v_reg.espec_nombre IN ('Ortopedia', 'Traumatología') THEN
      v_reg.multa_dia := v_multas(2);
    ELSIF v_reg.espec_nombre IN ('Inmunología', 'Otorrinolaringología') THEN
      v_reg.multa_dia := v_multas(3);
    ELSIF v_reg.espec_nombre IN ('Fisiatría', 'Medicina Interna') THEN
      v_reg.multa_dia := v_multas(4);
    ELSIF v_reg.espec_nombre = 'Medicina General' THEN
      v_reg.multa_dia := v_multas(5);
    ELSIF v_reg.espec_nombre = 'Psiquiatría Adultos' THEN
      v_reg.multa_dia := v_multas(6);
    ELSIF v_reg.espec_nombre IN ('Cirugía Digestiva', 'Reumatología') THEN
      v_reg.multa_dia := v_multas(7);
    ELSE
      v_reg.multa_dia := 0; 
    END IF;

    -- Monto multa total aplicando descuento tercera edad
    v_reg.monto_multa :=
      ROUND(v_reg.dias_mora * v_reg.multa_dia * (1 - (v_reg.pct_desc / 100)));

   
    INSERT INTO pago_moroso
      (pac_run, pac_dv_run, pac_nombre, ate_id,
       fecha_venc_pago, fecha_pago, dias_morosidad,
       especialidad_atencion, monto_multa)
    VALUES
      (v_reg.pac_run, v_reg.pac_dv, v_reg.pac_nombre, v_reg.ate_id,
       v_reg.fec_venc, v_reg.fec_pago, v_reg.dias_mora,
       SUBSTR(v_reg.espec_nombre, 1, 30), v_reg.monto_multa);

  END LOOP;

  CLOSE c_morosidad;

  COMMIT;

EXCEPTION
  WHEN OTHERS THEN
    
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR CASO 1: ' || SQLERRM);
    RAISE;
END;
/
-- SELECT 
SELECT
  pac_run,
  pac_dv_run,
  pac_nombre,
  ate_id,
  fecha_venc_pago,
  fecha_pago,
  dias_morosidad,
  REGEXP_REPLACE(
    TRANSLATE(especialidad_atencion,
      'ÁÉÍÓÚáéíóúÑñ',
      'AEIOUaeiouNn'
    ),
    '[^A-Za-z ]', ''
  ) AS especialidad_atencion,
  monto_multa
FROM pago_moroso
ORDER BY fecha_venc_pago ASC, pac_nombre ASC;

/*===============================================================
  CASO 2 – MEDICO_SERVICIO_COMUNIDAD (DEFINITIVO / COMPILA)
===============================================================*/

--===============================================================
-- 1) DROP 
--===============================================================
BEGIN
  EXECUTE IMMEDIATE 'DROP TABLE medico_servicio_comunidad CASCADE CONSTRAINTS';
EXCEPTION
  WHEN OTHERS THEN
    NULL; -- si no existe, ok
END;
/

CREATE TABLE medico_servicio_comunidad(
  unidad VARCHAR2(50) NOT NULL,
  run_medico VARCHAR2(15) NOT NULL,
  nombre_medico VARCHAR2(50) NOT NULL,
  correo_institucional VARCHAR2(30) NOT NULL,
  total_aten_medicas NUMBER(4) NOT NULL,
  destinacion VARCHAR2(60) NOT NULL
);

--===============================================================
-- 2) BLOQUE PL/SQL ANÓNIMO
--===============================================================
DECLARE

  v_dummy_unidad   unidad.nombre%TYPE;
  v_dummy_med_run  medico.med_run%TYPE;
  v_dummy_dv       medico.dv_run%TYPE;

  ---------------------------------------------------------------
  -- Año anterior dinámico (sin fechas fijas)
  ---------------------------------------------------------------
  v_anno_base NUMBER := EXTRACT(YEAR FROM SYSDATE) - 1;
  v_fec_ini   DATE   := TO_DATE('01-01-' || TO_CHAR(v_anno_base), 'DD-MM-YYYY');
  v_fec_fin   DATE   := ADD_MONTHS(v_fec_ini, 12) - 1;

 
  TYPE t_dest IS VARRAY(3) OF VARCHAR2(60);
  v_dest t_dest := t_dest(
    'Servicio de Atencion Primaria de Urgencia (SAPU)',
    'Hospitales del area de la Salud Publica',
    'Centros de Salud Familiar (CESFAM)'
  );

 
  CURSOR c_medicos IS
    SELECT
      u.nombre AS unidad,
      m.med_run,
      m.dv_run,
      (m.pnombre || ' ' || m.snombre || ' ' || m.apaterno || ' ' || m.amaterno) AS nombre_completo,
      m.apaterno,
      NVL(COUNT(a.ate_id), 0) AS total_atenciones
    FROM medico m
      JOIN unidad u ON u.uni_id = m.uni_id
      LEFT JOIN atencion a
        ON a.med_run = m.med_run
       AND a.fecha_atencion BETWEEN v_fec_ini AND v_fec_fin
    GROUP BY
      u.nombre, m.med_run, m.dv_run,
      m.pnombre, m.snombre, m.apaterno, m.amaterno
    ORDER BY u.nombre, m.apaterno;

  ---------------------------------------------------------------
  -- Registro PL/SQL 
  ---------------------------------------------------------------
  v_reg c_medicos%ROWTYPE;

  ---------------------------------------------------------------
  -- Auxiliares
  ---------------------------------------------------------------
  v_unidad_cmp VARCHAR2(120);
  v_max        NUMBER;
  v_correo     VARCHAR2(30);
  v_destino    VARCHAR2(60);

BEGIN
  ---------------------------------------------------------------
  -- 1) Máximo anual de atenciones (año anterior)
  ---------------------------------------------------------------
  SELECT MAX(cnt)
    INTO v_max
    FROM (
      SELECT NVL(COUNT(a.ate_id), 0) AS cnt
      FROM medico m
        LEFT JOIN atencion a
          ON a.med_run = m.med_run
         AND a.fecha_atencion BETWEEN v_fec_ini AND v_fec_fin
      GROUP BY m.med_run
    );

  ---------------------------------------------------------------
  -- 2) Procesar TODOS los médicos 
  ---------------------------------------------------------------
  OPEN c_medicos;
  LOOP
    FETCH c_medicos INTO v_reg;
    EXIT WHEN c_medicos%NOTFOUND;

    -- Solo médicos con MENOS del máximo (regla negocio)
    IF v_reg.total_atenciones < v_max THEN

      v_unidad_cmp := REGEXP_REPLACE(UPPER(TRIM(v_reg.unidad)), '[^A-Z]', '');

      -----------------------------------------------------------
      -- Correo institucional 
      -- 2 primeras letras unidad + 2 ultimas letras apaterno + 3 ultimos RUN
      -----------------------------------------------------------
      v_correo :=
        SUBSTR(UPPER(v_reg.unidad), 1, 2) ||
        SUBSTR(LOWER(v_reg.apaterno), GREATEST(LENGTH(v_reg.apaterno)-1,1), 2) ||
        SUBSTR(TO_CHAR(v_reg.med_run), -3) ||
        '@medicocktk.cl';

      -----------------------------------------------------------
      -- destinación 
      -----------------------------------------------------------
      IF (INSTR(v_unidad_cmp, 'ATENCI') > 0 AND INSTR(v_unidad_cmp, 'ADUL') > 0)
   OR INSTR(v_unidad_cmp, 'AMBUL') > 0 THEN
  -- Atencion Adulto / Ambulatoria -> SAPU
  v_destino := v_dest(1);

ELSIF INSTR(v_unidad_cmp, 'URGEN') > 0 THEN
  -- Atencion Urgencia -> depende del total
  IF v_reg.total_atenciones <= 3 THEN
    v_destino := v_dest(1);
  ELSE
    v_destino := v_dest(2);
  END IF;

ELSIF INSTR(v_unidad_cmp, 'CARD') > 0
   OR INSTR(v_unidad_cmp, 'ONCOL') > 0 THEN
  -- Cardiologia / Oncologia -> Hospitales
  v_destino := v_dest(2);

ELSIF INSTR(v_unidad_cmp, 'CIRUG') > 0 THEN
  -- Cirugia y Cirugia Plastica -> depende del total
  IF v_reg.total_atenciones <= 3 THEN
    v_destino := v_dest(1);
  ELSE
    v_destino := v_dest(2);
  END IF;

ELSIF (INSTR(v_unidad_cmp, 'PACIENTE') > 0 AND INSTR(v_unidad_cmp, 'CRT') > 0) THEN
  -- Paciente Critico -> Hospitales
  v_destino := v_dest(2);

ELSIF (INSTR(v_unidad_cmp, 'PSIQUI') > 0
       AND INSTR(v_unidad_cmp, 'SALUD') > 0
       AND INSTR(v_unidad_cmp, 'MENTAL') > 0) THEN
  -- Psiquiatria y Salud Mental -> CESFAM
  v_destino := v_dest(3);

ELSIF INSTR(v_unidad_cmp, 'TRAUMA') > 0 THEN
  -- Traumatologia Adulto -> depende del total
  IF v_reg.total_atenciones <= 3 THEN
    v_destino := v_dest(1);
  ELSE
    v_destino := v_dest(2);
  END IF;

ELSE
  v_destino := 'SIN DEFINIR';
END IF;


      INSERT INTO medico_servicio_comunidad
        (unidad, run_medico, nombre_medico, correo_institucional, total_aten_medicas, destinacion)
      VALUES
        (v_reg.unidad,
         TO_CHAR(v_reg.med_run) || '-' || v_reg.dv_run,
         v_reg.nombre_completo,
         v_correo,
         v_reg.total_atenciones,
         v_destino);

    END IF;
  END LOOP;

  CLOSE c_medicos;

  COMMIT;

EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR CASO 2: ' || SQLERRM);
    RAISE;
END;
/

SELECT unidad, run_medico, nombre_medico, correo_institucional, total_aten_medicas, destinacion
FROM medico_servicio_comunidad
ORDER BY unidad, nombre_medico;


