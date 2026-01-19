VAR b_fecha_proceso VARCHAR2(10);
EXEC :b_fecha_proceso := TO_CHAR(SYSDATE,'DD-MM-YYYY');

/* ============================================================
   BLOQUE PL/SQL ANONIMO - SUMATIVA 1
   Genera NOMBRE_USUARIO y CLAVE_USUARIO por empleado
   y almacena resultados en USUARIO_CLAVE.
   ============================================================ */
DECLARE
 
  v_fecha_proceso  DATE := TO_DATE(:b_fecha_proceso, 'DD-MM-YYYY');

/* =========================================================
    Variables con %TYPE
   ========================================================= */
  v_id_emp         empleado.id_emp%TYPE;
  v_numrun         empleado.numrun_emp%TYPE;
  v_dv             empleado.dvrun_emp%TYPE;
  v_pnombre        empleado.pnombre_emp%TYPE;
  v_appaterno      empleado.appaterno_emp%TYPE;
  v_apmaterno      empleado.apmaterno_emp%TYPE;
  v_sueldo_base    empleado.sueldo_base%TYPE;
  v_fec_nac        empleado.fecha_nac%TYPE;
  v_fec_contrato   empleado.fecha_contrato%TYPE;

  /* Nombre de estado civil */
  v_nom_est_civil  estado_civil.nombre_estado_civil%TYPE;

  /* Salidas requeridas */
  v_nombre_empleado  VARCHAR2(60);
  v_nombre_usuario   VARCHAR2(30);
  v_clave_usuario    VARCHAR2(40);

  /* Auxiliares de reglas */
  v_annos_trab     NUMBER(3);
  v_letras_ap      VARCHAR2(2);

  /* Blindajes para enteros */
  v_sueldo_ent     NUMBER;
  v_ult3           NUMBER;
  v_ult3_adj       NUMBER;
  v_run_txt        VARCHAR2(20);

  /* Control transaccional */
  v_total_empleados  NUMBER := 0;
  v_cont_procesados  NUMBER := 0;

BEGIN
  /* =========================================================
     SENTENCIA SQL DOCUMENTADA #1:
     Limpiar la tabla de resultados antes de ejecutar.
     Se usa SQL dinámico porque TRUNCATE no se permite directo en PL/SQL.
     Esto permite ejecutar el bloque todas las veces que se requiera.
     ========================================================= */
  EXECUTE IMMEDIATE 'TRUNCATE TABLE USUARIO_CLAVE';

  /* Cantidad total de empleados para validar proceso completo */
  SELECT COUNT(*)
  INTO v_total_empleados
  FROM empleado;


  FOR r IN (SELECT id_emp FROM empleado ORDER BY id_emp) LOOP
    BEGIN
      /* =====================================================
         SENTENCIA SQL DOCUMENTADA #2 (EFICIENCIA):
         Trae toda la información necesaria en UNA sola consulta
         usando JOIN (evita hacer 2 SELECT por empleado).
         ===================================================== */
      SELECT  e.id_emp, e.numrun_emp, e.dvrun_emp,
              e.pnombre_emp, e.appaterno_emp, e.apmaterno_emp,
              e.sueldo_base, e.fecha_nac, e.fecha_contrato,
              ec.nombre_estado_civil
      INTO    v_id_emp, v_numrun, v_dv,
              v_pnombre, v_appaterno, v_apmaterno,
              v_sueldo_base, v_fec_nac, v_fec_contrato,
              v_nom_est_civil
      FROM empleado e
      JOIN estado_civil ec
        ON ec.id_estado_civil = e.id_estado_civil
      WHERE e.id_emp = r.id_emp;

      /* Nombre completo*/
      v_nombre_empleado := v_pnombre || ' ' || v_appaterno || ' ' || v_apmaterno;

      /* =====================================================
         Años trabajados: cálculo paramétrico con v_fecha_proceso
         y redondeado a entero.
         ===================================================== */
      v_annos_trab := TRUNC(MONTHS_BETWEEN(v_fecha_proceso, v_fec_contrato) / 12);

      /* =====================================================
         sueldo en entero para asegurar cálculos sin decimales (Blindaje) 
         ===================================================== */
      v_sueldo_ent := TRUNC(v_sueldo_base);

      /* =====================================================
         RUN con LPAD para garantizar extracción del 3er dígito
         ===================================================== */
      v_run_txt := LPAD(TO_CHAR(v_numrun), 8, '0');

      /* -------------------------
         Generación NOMBRE_USUARIO 
         ------------------------- */
      v_nombre_usuario :=
           LOWER(SUBSTR(v_nom_est_civil,1,1))      -- a) 1ra letra estado civil
        || LOWER(SUBSTR(v_pnombre,1,3))            -- b) 3 primeras letras del nombre
        || LENGTH(v_pnombre)                       -- c) largo del primer nombre
        || '*'                                     -- d) asterisco
        || MOD(v_sueldo_ent,10)                    -- e) último dígito sueldo base
        || v_dv                                    -- f) dv del run
        || v_annos_trab;                           -- g) años trabajados

      IF v_annos_trab < 10 THEN                    -- h) si <10 años, agregar X
        v_nombre_usuario := v_nombre_usuario || 'X';
      END IF;

      /* -------------------------
         Letras del apellido paterno según estado civil
         ------------------------- */
      IF v_nom_est_civil IN ('CASADO', 'ACUERDO DE UNION CIVIL') THEN
        v_letras_ap := LOWER(SUBSTR(v_appaterno,1,2));
      ELSIF v_nom_est_civil IN ('DIVORCIADO', 'SOLTERO') THEN
        v_letras_ap := LOWER(SUBSTR(v_appaterno,1,1) || SUBSTR(v_appaterno,-1,1));
      ELSIF v_nom_est_civil = 'VIUDO' THEN
        v_letras_ap := LOWER(SUBSTR(v_appaterno,-3,1) || SUBSTR(v_appaterno,-2,1));
      ELSIF v_nom_est_civil = 'SEPARADO' THEN
        v_letras_ap := LOWER(SUBSTR(v_appaterno,-2,2));
      ELSE
        v_letras_ap := LOWER(SUBSTR(v_appaterno,1,2)); -- respaldo
      END IF;

      /* =====================================================
         PL/SQL DOCUMENTADO #2:
         Control de "tres últimos dígitos del sueldo base - 1"
         (evita valores negativos y mantiene 3 dígitos)
         ===================================================== */
      v_ult3 := MOD(v_sueldo_ent, 1000);

      IF v_ult3 = 0 THEN
        v_ult3_adj := 999;
      ELSE
        v_ult3_adj := v_ult3 - 1;
      END IF;

      /* -------------------------
         Construcción CLAVE_USUARIO 
         ------------------------- */
      v_clave_usuario :=
           SUBSTR(v_run_txt,3,1)                       -- a) 3er dígito del run
        || (EXTRACT(YEAR FROM v_fec_nac) + 2)           -- b) año nac + 2
        || LPAD(TO_CHAR(v_ult3_adj), 3, '0')            -- c) últimos 3 del sueldo - 1 (ajustado)
        || v_letras_ap                                  -- d) 2 letras apellido segun EC
        || v_id_emp                                     -- e) identificación del empleado
        || TO_CHAR(v_fecha_proceso,'MMYYYY');           -- f) mes y año de BD/ejecución

      /* Insert en tabla USUARIO_CLAVE */
      INSERT INTO usuario_clave
        (id_emp, numrun_emp, dvrun_emp, nombre_empleado, nombre_usuario, clave_usuario)
      VALUES
        (v_id_emp, v_numrun, v_dv, v_nombre_empleado, v_nombre_usuario, v_clave_usuario);

      v_cont_procesados := v_cont_procesados + 1;

    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20001, 'No se encontraron datos para id_emp=' || r.id_emp);
      WHEN OTHERS THEN
        RAISE;
    END;
  END LOOP;

  /* =========================================================
     Control transaccional:
     COMMIT solo si se procesaron todos los empleados.
     Si no, ROLLBACK y error.
     ========================================================= */
  IF v_cont_procesados = v_total_empleados THEN
    COMMIT;
  ELSE
    ROLLBACK;
    RAISE_APPLICATION_ERROR(-20002,
      'Proceso incompleto. Procesados='||v_cont_procesados||' de Total='||v_total_empleados);
  END IF;

END;
/
 /*==============================================================
    CONSULTA
 ================================================================*/
SELECT *
FROM usuario_clave
ORDER BY id_emp;