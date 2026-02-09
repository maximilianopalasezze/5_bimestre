SET SERVEROUTPUT ON;

-- ============================================================
-- VARIABLE BIND: periodo de ejecución (año)
-- ============================================================
VAR v_periodo NUMBER;

-- Por defecto, procesar el año actual (paramétrico, NO fecha fija)
EXEC :v_periodo := EXTRACT(YEAR FROM SYSDATE);

DECLARE
  /* ============================================================
     SUMATIVA SEMANA 5 - APORTE SBIF
     Proceso: Genera DETALLE_APORTE_SBIF y RESUMEN_APORTE_SBIF
     ============================================================ */

  /* VARRAY con los CÓDIGOS de tipo de transacción  */
  TYPE t_varray_cod IS VARRAY(2) OF NUMBER;
  v_codigos t_varray_cod := t_varray_cod(102, 103); -- 102=Avance, 103=S?per Avance (según tus SELECT)

  /* Registro PL/SQL para fila del cursor detalle */
  TYPE r_det IS RECORD(
    numrun             cliente.numrun%TYPE,
    dvrun              cliente.dvrun%TYPE,
    nro_tarjeta        tarjeta_cliente.nro_tarjeta%TYPE,
    nro_transaccion    transaccion_tarjeta_cliente.nro_transaccion%TYPE,
    fecha_transaccion  transaccion_tarjeta_cliente.fecha_transaccion%TYPE,
    tipo_transaccion   tipo_transaccion_tarjeta.nombre_tptran_tarjeta%TYPE,
    monto_total        transaccion_tarjeta_cliente.monto_total_transaccion%TYPE
  );
  v_det r_det;

  /* ------------------------------------------------------------
     CURSOR 1 (SIN PARÁMETRO)
     - Trae DETALLE ordenado por fecha y RUN 
     - Filtrar por año usando bind
     ------------------------------------------------------------ */
  CURSOR c_detalle IS
    SELECT
      c.numrun,
      c.dvrun,
      t.nro_tarjeta,
      t.nro_transaccion,
      t.fecha_transaccion,
      tt.nombre_tptran_tarjeta AS tipo_transaccion,
      t.monto_total_transaccion AS monto_total
    FROM transaccion_tarjeta_cliente t
    JOIN tarjeta_cliente tc
      ON tc.nro_tarjeta = t.nro_tarjeta
    JOIN cliente c
      ON c.numrun = tc.numrun
    JOIN tipo_transaccion_tarjeta tt
      ON tt.cod_tptran_tarjeta = t.cod_tptran_tarjeta
    WHERE EXTRACT(YEAR FROM t.fecha_transaccion) = :v_periodo
      AND t.cod_tptran_tarjeta IN (102, 103)
    ORDER BY t.fecha_transaccion ASC, c.numrun ASC;

  /* ------------------------------------------------------------
     CURSOR 2 (CON PARÁMETRO)
     - Para RESUMEN por tipo (código)
     ------------------------------------------------------------ */
  CURSOR c_resumen_por_tipo(p_cod NUMBER) IS
    SELECT
      TO_CHAR(t.fecha_transaccion,'MMYYYY') AS mes_anno,
      t.monto_total_transaccion            AS monto_total
    FROM transaccion_tarjeta_cliente t
    WHERE EXTRACT(YEAR FROM t.fecha_transaccion) = :v_periodo
      AND t.cod_tptran_tarjeta = p_cod
    ORDER BY TO_CHAR(t.fecha_transaccion,'MMYYYY') ASC;

  /* ============================
     Variables de cálculo
     ============================ */
  v_porcentaje_aporte  tramo_aporte_sbif.porc_aporte_sbif%TYPE;
  v_aporte             NUMBER;

  /* (m) Control de commit: total esperado vs procesados */
  v_total_registros    NUMBER := 0;
  v_procesados_det     NUMBER := 0;

  /* Acumuladores para resumen */
  v_mes_actual         VARCHAR2(6);
  v_monto_mes          NUMBER;
  v_aporte_mes         NUMBER;

  /* ============================
      EXCEPCIONES
     ============================ */

  -- 1) Predefinida: NO_DATA_FOUND (la atraparemos en búsquedas de tramo)
  -- 2) No predefinida: ORA-00942 mapeada con PRAGMA
  e_tabla_no_existe EXCEPTION;
  PRAGMA EXCEPTION_INIT(e_tabla_no_existe, -942);

  -- 3) Definida por el usuario
  e_tramo_no_definido EXCEPTION;
  e_commit_bloqueado  EXCEPTION;

BEGIN
  /* TRUNCAR tablas para ejecución repetible */
  EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_APORTE_SBIF';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE RESUMEN_APORTE_SBIF';

  /* Total esperado a procesar en DETALLE */
  SELECT COUNT(*)
    INTO v_total_registros
  FROM transaccion_tarjeta_cliente t
  WHERE EXTRACT(YEAR FROM t.fecha_transaccion) = :v_periodo
    AND t.cod_tptran_tarjeta IN (102, 103);

  /* ============================================================
     1) PROCESO DETALLE: inserta DETALLE_APORTE_SBIF
     ============================================================ */
  OPEN c_detalle;
  LOOP
    FETCH c_detalle INTO
      v_det.numrun,
      v_det.dvrun,
      v_det.nro_tarjeta,
      v_det.nro_transaccion,
      v_det.fecha_transaccion,
      v_det.tipo_transaccion,
      v_det.monto_total;

    EXIT WHEN c_detalle%NOTFOUND;

    v_procesados_det := v_procesados_det + 1;

    /* Buscar porcentaje de aporte por tramo (en PL/SQL, NO en SELECT del cursor) */
    BEGIN
      SELECT porc_aporte_sbif
        INTO v_porcentaje_aporte
      FROM tramo_aporte_sbif
      WHERE ROUND(v_det.monto_total) BETWEEN tramo_inf_av_sav AND tramo_sup_av_sav;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        -- Excepción predefinida controlada: no hay tramo aplicable
        RAISE e_tramo_no_definido;
    END;

    /* Cálculo aporte: monto_total redondeado * porcentaje, resultado entero */
    v_aporte := ROUND( ROUND(v_det.monto_total) * (v_porcentaje_aporte / 100) );

    /* Insert en DETALLE */
    INSERT INTO detalle_aporte_sbif(
      numrun, dvrun, nro_tarjeta, nro_transaccion,
      fecha_transaccion, tipo_transaccion,
      monto_transaccion, aporte_sbif
    ) VALUES (
      v_det.numrun, v_det.dvrun, v_det.nro_tarjeta, v_det.nro_transaccion,
      v_det.fecha_transaccion, v_det.tipo_transaccion,
      ROUND(v_det.monto_total), v_aporte
    );
  END LOOP;
  CLOSE c_detalle;

  /* ============================================================
     2) PROCESO RESUMEN: inserta RESUMEN_APORTE_SBIF
     - Cursor con parámetro (por código)
     - Totaliza por mes y por tipo
     ============================================================ */
  FOR i IN 1 .. v_codigos.COUNT LOOP
    DECLARE
      v_nombre_tipo tipo_transaccion_tarjeta.nombre_tptran_tarjeta%TYPE;
    BEGIN
      /* Obtener el nombre del tipo según código */
      SELECT nombre_tptran_tarjeta
        INTO v_nombre_tipo
      FROM tipo_transaccion_tarjeta
      WHERE cod_tptran_tarjeta = v_codigos(i);

      v_mes_actual := NULL;
      v_monto_mes  := 0;
      v_aporte_mes := 0;

      OPEN c_resumen_por_tipo(v_codigos(i));
      LOOP
        DECLARE
          v_mes  VARCHAR2(6);
          v_mt   NUMBER;
          v_pct  NUMBER;
          v_ap   NUMBER;
        BEGIN
          FETCH c_resumen_por_tipo INTO v_mes, v_mt;
          EXIT WHEN c_resumen_por_tipo%NOTFOUND;

          /* Si cambia el mes, guardamos el mes anterior */
          IF v_mes_actual IS NOT NULL AND v_mes <> v_mes_actual THEN
            INSERT INTO resumen_aporte_sbif(
              mes_anno, tipo_transaccion, monto_total_transacciones, aporte_total_abif
            ) VALUES (
              v_mes_actual, v_nombre_tipo, ROUND(v_monto_mes), ROUND(v_aporte_mes)
            );

            v_monto_mes  := 0;
            v_aporte_mes := 0;
          END IF;

          v_mes_actual := v_mes;

          /* Acumular monto mensual (entero) */
          v_monto_mes := v_monto_mes + ROUND(v_mt);

          /* Calcular aporte por transacción en PL/SQL */
          BEGIN
            SELECT porc_aporte_sbif
              INTO v_pct
            FROM tramo_aporte_sbif
            WHERE ROUND(v_mt) BETWEEN tramo_inf_av_sav AND tramo_sup_av_sav;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              RAISE e_tramo_no_definido;
          END;

          v_ap := ROUND( ROUND(v_mt) * (v_pct / 100) );
          v_aporte_mes := v_aporte_mes + v_ap;
        END;
      END LOOP;
      CLOSE c_resumen_por_tipo;

      /* Insertar último mes pendiente */
      IF v_mes_actual IS NOT NULL THEN
        INSERT INTO resumen_aporte_sbif(
          mes_anno, tipo_transaccion, monto_total_transacciones, aporte_total_abif
        ) VALUES (
          v_mes_actual, v_nombre_tipo, ROUND(v_monto_mes), ROUND(v_aporte_mes)
        );
      END IF;

    END;
  END LOOP;

  /* ============================================================
     COMMIT solo si se procesaron todos los registros esperados
     ============================================================ */
  IF v_procesados_det = v_total_registros THEN
    COMMIT;
    DBMS_OUTPUT.PUT_LINE('OK: Proceso finalizado. Registros procesados: ' || v_procesados_det);
  ELSE
    ROLLBACK;
    RAISE e_commit_bloqueado;
  END IF;

EXCEPTION
  WHEN e_tramo_no_definido THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR: Falta tramo en TRAMO_APORTE_SBIF para algún monto. ROLLBACK.');

  WHEN e_tabla_no_existe THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR ORA-00942: Falta tabla (modelo no cargado/esquema). ROLLBACK.');

  WHEN e_commit_bloqueado THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR: No se procesaron todos los registros. COMMIT bloqueado. ROLLBACK.');

  WHEN OTHERS THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR GENERAL: ' || SQLERRM || ' (Se hizo ROLLBACK)');
END;
/
-- ============================================================
-- SELECTS para visualizar resultados
-- ============================================================

-- (Opcional) Formato para que se vea más ordenado
COLUMN mes_anno FORMAT A8
COLUMN tipo_transaccion FORMAT A28


SELECT
  numrun,
  dvrun,
  nro_tarjeta,
  nro_transaccion,
  fecha_transaccion,
  CASE
    WHEN tipo_transaccion LIKE 'S%per Avance en Efectivo' THEN 'Súper Avance en Efectivo'
    ELSE tipo_transaccion
  END AS tipo_transaccion,
  monto_transaccion,
  aporte_sbif
FROM detalle_aporte_sbif
ORDER BY fecha_transaccion ASC, numrun ASC;


SELECT
  mes_anno,
  CASE
    WHEN tipo_transaccion LIKE 'S%per Avance en Efectivo' THEN 'Súper Avance en Efectivo'
    ELSE tipo_transaccion
  END AS tipo_transaccion,
  monto_total_transacciones,
  aporte_total_abif
FROM resumen_aporte_sbif
ORDER BY mes_anno ASC, tipo_transaccion ASC;