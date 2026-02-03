* ============================================================
   PRY2206 - PRÁCTICA 4
   Actividad Semana 4: Cursores explícitos complejos (CASO 1 y CASO 2)
   ============================================================ */

SET SERVEROUTPUT ON;


VAR v_tramo1_desde NUMBER;
VAR v_tramo1_hasta NUMBER;
VAR v_tramo2_desde NUMBER;
VAR v_tramo2_hasta NUMBER;


VAR v_puntos_normal NUMBER;
VAR v_puntos_extra1 NUMBER;
VAR v_puntos_extra2 NUMBER;
VAR v_puntos_extra3 NUMBER;

VAR v_cod_duena_casa NUMBER;
VAR v_cod_pensionado NUMBER;
VAR v_cod_tercera_edad NUMBER;


EXEC :v_tramo1_desde := 500000;
EXEC :v_tramo1_hasta := 700000;
EXEC :v_tramo2_desde := 700001;
EXEC :v_tramo2_hasta := 900000;

EXEC :v_puntos_normal := 250;
EXEC :v_puntos_extra1 := 300;
EXEC :v_puntos_extra2 := 550;
EXEC :v_puntos_extra3 := 700;

EXEC :v_cod_duena_casa := 1;
EXEC :v_cod_pensionado := 2;
EXEC :v_cod_tercera_edad := 3;

PROMPT ============================================================
PROMPT ======================= CASO 1 ============================
PROMPT ============================================================

DECLARE
  /* Año objetivo: año anterior al de ejecución  */
  v_anio_objetivo NUMBER := EXTRACT(YEAR FROM ADD_MONTHS(SYSDATE, -12));

   
  TYPE t_varray_puntos IS VARRAY(4) OF NUMBER;
  v_puntos t_varray_puntos := t_varray_puntos(
    :v_puntos_normal, -- 1) puntos normales
    :v_puntos_extra1, -- 2) extra tramo 1
    :v_puntos_extra2, -- 3) extra tramo 2
    :v_puntos_extra3  -- 4) extra tramo 3
  );

 
  TYPE r_tx IS RECORD (
    numrun            NUMBER,
    dvrun             VARCHAR2(1),
    nro_tarjeta       NUMBER,
    nro_transaccion   NUMBER,
    fecha_transaccion DATE,
    tipo_transaccion  VARCHAR2(200),
    monto_transaccion NUMBER
  );
  v_tx r_tx;

  
  TYPE t_refcur IS REF CURSOR;
  rc_meses t_refcur;
  v_mes_anno VARCHAR2(6); -- formato MMYYYY

 
  CURSOR c_detalle_mes(p_mes_anno VARCHAR2) IS
    SELECT
      c.numrun,
      c.dvrun,
      t.nro_tarjeta,
      t.nro_transaccion,
      t.fecha_transaccion,
      tt.nombre_tptran_tarjeta AS tipo_transaccion,
      t.monto_transaccion
    FROM transaccion_tarjeta_cliente t
    JOIN tarjeta_cliente tc
      ON tc.nro_tarjeta = t.nro_tarjeta
    JOIN cliente c
      ON c.numrun = tc.numrun
    JOIN tipo_transaccion_tarjeta tt
      ON tt.cod_tptran_tarjeta = t.cod_tptran_tarjeta
    WHERE EXTRACT(YEAR FROM t.fecha_transaccion) = v_anio_objetivo
      AND TO_CHAR(t.fecha_transaccion,'MMYYYY') = p_mes_anno
      AND tt.nombre_tptran_tarjeta IN (
        'Compras Tiendas Retail o Asociadas',
        'Avance en Efectivo',
        'Súper Avance en Efectivo'
      )
   
    ORDER BY t.fecha_transaccion ASC, c.numrun ASC, t.nro_transaccion ASC;

  /* Variables de cálculo */
  v_cod_tipo_cliente    NUMBER;
  v_total_anual_cliente NUMBER;
  v_multiplo_100k       NUMBER;
  v_puntos_base         NUMBER;
  v_puntos_extra        NUMBER;
  v_puntos_total        NUMBER;

  /* Acumuladores del resumen mensual */
  v_monto_compras  NUMBER;
  v_pts_compras    NUMBER;
  v_monto_avances  NUMBER;
  v_pts_avances    NUMBER;
  v_monto_savances NUMBER;
  v_pts_savances   NUMBER;

BEGIN
  
  EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_PUNTOS_TARJETA_CATB';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE RESUMEN_PUNTOS_TARJETA_CATB';

  
  OPEN rc_meses FOR
    SELECT DISTINCT TO_CHAR(t.fecha_transaccion,'MMYYYY') AS mes_anno
    FROM transaccion_tarjeta_cliente t
    JOIN tipo_transaccion_tarjeta tt
      ON tt.cod_tptran_tarjeta = t.cod_tptran_tarjeta
    WHERE EXTRACT(YEAR FROM t.fecha_transaccion) = v_anio_objetivo
      AND tt.nombre_tptran_tarjeta IN (
        'Compras Tiendas Retail o Asociadas',
        'Avance en Efectivo',
        'Súper Avance en Efectivo'
      )
    
    ORDER BY mes_anno ASC;

  LOOP
    FETCH rc_meses INTO v_mes_anno;
    EXIT WHEN rc_meses%NOTFOUND;

    /* Reiniciar acumuladores por cada mes */
    v_monto_compras  := 0; v_pts_compras  := 0;
    v_monto_avances  := 0; v_pts_avances  := 0;
    v_monto_savances := 0; v_pts_savances := 0;

    /* Abrir cursor detalle del mes */
    OPEN c_detalle_mes(v_mes_anno);
    LOOP
      FETCH c_detalle_mes INTO
        v_tx.numrun, v_tx.dvrun, v_tx.nro_tarjeta, v_tx.nro_transaccion,
        v_tx.fecha_transaccion, v_tx.tipo_transaccion, v_tx.monto_transaccion;
      EXIT WHEN c_detalle_mes%NOTFOUND;

      /* Obtener tipo de cliente (para decidir si aplica puntos extra) */
      SELECT cod_tipo_cliente
        INTO v_cod_tipo_cliente
      FROM cliente
      WHERE numrun = v_tx.numrun
        AND dvrun  = v_tx.dvrun;

      /*  Cálculo en PL/SQL: puntos base por cada 100.000 */
      v_multiplo_100k := FLOOR(v_tx.monto_transaccion / 100000);
      v_puntos_base   := v_multiplo_100k * v_puntos(1);
      v_puntos_extra  := 0;

      /*  Puntos extra solo para dueña de casa / pensionado / tercera edad */
      IF v_cod_tipo_cliente IN (:v_cod_duena_casa, :v_cod_pensionado, :v_cod_tercera_edad) THEN

        /* Total anual del cliente (año objetivo) para evaluar tramo */
        SELECT NVL(SUM(t2.monto_transaccion),0)
          INTO v_total_anual_cliente
        FROM transaccion_tarjeta_cliente t2
        JOIN tarjeta_cliente tc2
          ON tc2.nro_tarjeta = t2.nro_tarjeta
        JOIN tipo_transaccion_tarjeta tt2
          ON tt2.cod_tptran_tarjeta = t2.cod_tptran_tarjeta
        WHERE tc2.numrun = v_tx.numrun
          AND EXTRACT(YEAR FROM t2.fecha_transaccion) = v_anio_objetivo
          AND tt2.nombre_tptran_tarjeta IN (
            'Compras Tiendas Retail o Asociadas',
            'Avance en Efectivo',
            'Súper Avance en Efectivo'
          );

        /* Tramos ingresados paramétricamente (BIND) + condicional PL/SQL */
        IF v_total_anual_cliente BETWEEN :v_tramo1_desde AND :v_tramo1_hasta THEN
          v_puntos_extra := v_multiplo_100k * v_puntos(2);
        ELSIF v_total_anual_cliente BETWEEN :v_tramo2_desde AND :v_tramo2_hasta THEN
          v_puntos_extra := v_multiplo_100k * v_puntos(3);
        ELSIF v_total_anual_cliente > :v_tramo2_hasta THEN
          v_puntos_extra := v_multiplo_100k * v_puntos(4);
        END IF;
      END IF;

      v_puntos_total := v_puntos_base + v_puntos_extra;

      /* Insert detalle con LISTA DE COLUMNAS  */
      INSERT INTO detalle_puntos_tarjeta_catb(
        numrun, dvrun, nro_tarjeta, nro_transaccion,
        fecha_transaccion, tipo_transaccion,
        monto_transaccion, puntos_allthebest
      ) VALUES (
        v_tx.numrun, v_tx.dvrun, v_tx.nro_tarjeta, v_tx.nro_transaccion,
        v_tx.fecha_transaccion, v_tx.tipo_transaccion,
        v_tx.monto_transaccion, v_puntos_total
      );

      /* Acumular resumen por tipo de transacción */
      IF v_tx.tipo_transaccion = 'Compras Tiendas Retail o Asociadas' THEN
        v_monto_compras := v_monto_compras + v_tx.monto_transaccion;
        v_pts_compras   := v_pts_compras   + v_puntos_total;
      ELSIF v_tx.tipo_transaccion = 'Avance en Efectivo' THEN
        v_monto_avances := v_monto_avances + v_tx.monto_transaccion;
        v_pts_avances   := v_pts_avances   + v_puntos_total;
      ELSIF v_tx.tipo_transaccion = 'Súper Avance en Efectivo' THEN
        v_monto_savances := v_monto_savances + v_tx.monto_transaccion;
        v_pts_savances   := v_pts_savances   + v_puntos_total;
      END IF;

    END LOOP;
    CLOSE c_detalle_mes;

    /* Insert resumen del mes con LISTA DE COLUMNAS */
    INSERT INTO resumen_puntos_tarjeta_catb(
      mes_anno,
      monto_total_compras, total_puntos_compras,
      monto_total_avances, total_puntos_avances,
      monto_total_savances, total_puntos_savances
    ) VALUES (
      v_mes_anno,
      v_monto_compras, v_pts_compras,
      v_monto_avances, v_pts_avances,
      v_monto_savances, v_pts_savances
    );

  END LOOP;
  CLOSE rc_meses;

  COMMIT;
  DBMS_OUTPUT.PUT_LINE('CASO 1 OK - Año procesado: ' || v_anio_objetivo);

EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR CASO 1: ' || SQLERRM);
    RAISE;
END;
/

PROMPT ============================================================
PROMPT ======================= CASO 2 ============================
PROMPT ============================================================

DECLARE
  /* Año objetivo: año actual */
  v_anio_objetivo NUMBER := EXTRACT(YEAR FROM SYSDATE);

  /* Cursor explícito 1: meses y tipo de transacción (Avance / Súper Avance) */
  CURSOR c_meses_tipo IS
    SELECT
      TO_CHAR(t.fecha_transaccion,'MMYYYY') AS mes_anno,
      tt.nombre_tptran_tarjeta              AS tipo_transaccion
    FROM transaccion_tarjeta_cliente t
    JOIN tipo_transaccion_tarjeta tt
      ON tt.cod_tptran_tarjeta = t.cod_tptran_tarjeta
    WHERE EXTRACT(YEAR FROM t.fecha_transaccion) = v_anio_objetivo
      AND tt.nombre_tptran_tarjeta IN ('Avance en Efectivo','Súper Avance en Efectivo')
    GROUP BY TO_CHAR(t.fecha_transaccion,'MMYYYY'), tt.nombre_tptran_tarjeta
    /* Orden resumen: mes/año y tipo */
    ORDER BY mes_anno ASC, tt.nombre_tptran_tarjeta ASC;

 
  CURSOR c_detalle(p_mes_anno VARCHAR2, p_tipo VARCHAR2) IS
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
    WHERE EXTRACT(YEAR FROM t.fecha_transaccion) = v_anio_objetivo
      AND TO_CHAR(t.fecha_transaccion,'MMYYYY') = p_mes_anno
      AND tt.nombre_tptran_tarjeta = p_tipo
    /* (PAUTA) Orden detalle: fecha y run */
    ORDER BY t.fecha_transaccion ASC, c.numrun ASC;

  /* Variables para FETCH */
  v_numrun NUMBER;
  v_dvrun  VARCHAR2(1);
  v_nro_tarjeta NUMBER;
  v_nro_transaccion NUMBER;
  v_fecha DATE;
  v_tipo  VARCHAR2(200);
  v_monto_total NUMBER;

  /* Variables para cálculo de aporte */
  v_porc   NUMBER;
  v_aporte NUMBER;

  /* Acumuladores resumen mensual */
  v_monto_total_mes  NUMBER;
  v_aporte_total_mes NUMBER;

BEGIN
  /* TRUNCATE para proceso repetible */
  EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_APORTE_SBIF';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE RESUMEN_APORTE_SBIF';

  /* Recorrer por mes y tipo */
  FOR r IN c_meses_tipo LOOP
    v_monto_total_mes  := 0;
    v_aporte_total_mes := 0;

    OPEN c_detalle(r.mes_anno, r.tipo_transaccion);
    LOOP
      FETCH c_detalle INTO
        v_numrun, v_dvrun, v_nro_tarjeta, v_nro_transaccion,
        v_fecha, v_tipo, v_monto_total;
      EXIT WHEN c_detalle%NOTFOUND;

      /* Buscar porcentaje de aporte según tramo en TRAMO_APORTE_SBIF */
      BEGIN
        SELECT x.porc_aporte_sbif
          INTO v_porc
        FROM tramo_aporte_sbif x
        WHERE v_monto_total BETWEEN x.tramo_inf_av_sav AND x.tramo_sup_av_sav;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
        
          BEGIN
            SELECT x.porc_aporte_sbif
              INTO v_porc
            FROM tramo_aporte_sbif x
            WHERE v_monto_total >= x.tramo_inf_av_sav
              AND x.tramo_sup_av_sav IS NULL;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              v_porc := 0;
          END;
      END;

     
      v_aporte := ROUND(v_monto_total * (v_porc/100));

     
      INSERT INTO detalle_aporte_sbif(
        numrun, dvrun, nro_tarjeta, nro_transaccion,
        fecha_transaccion, tipo_transaccion,
        monto_transaccion, aporte_sbif
      ) VALUES (
        v_numrun, v_dvrun, v_nro_tarjeta, v_nro_transaccion,
        v_fecha, v_tipo,
        v_monto_total, v_aporte
      );

      /* Acumular para resumen */
      v_monto_total_mes  := v_monto_total_mes  + v_monto_total;
      v_aporte_total_mes := v_aporte_total_mes + v_aporte;

    END LOOP;
    CLOSE c_detalle;

    /* Insert resumen por mes/tipo */
    INSERT INTO resumen_aporte_sbif(
      mes_anno, tipo_transaccion,
      monto_total_transacciones, aporte_total_abif
    ) VALUES (
      r.mes_anno, r.tipo_transaccion,
      v_monto_total_mes, v_aporte_total_mes
    );

  END LOOP;

  COMMIT;
  DBMS_OUTPUT.PUT_LINE('CASO 2 OK - Año procesado: ' || v_anio_objetivo);

EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR CASO 2: ' || SQLERRM);
    RAISE;
END;
/

PROMPT ============================================================
PROMPT ================== SELECTS DE VALIDACIÓN ==================
PROMPT ============================================================

PROMPT === VALIDACIÓN CASO 1 (RESUMEN) ===
SELECT *
FROM resumen_puntos_tarjeta_catb
ORDER BY mes_anno;

PROMPT === VALIDACIÓN CASO 1 (DETALLE) ===
SELECT *
FROM detalle_puntos_tarjeta_catb
ORDER BY fecha_transaccion, numrun, nro_transaccion;

PROMPT === VALIDACIÓN CASO 2 (RESUMEN) ===
SELECT *
FROM resumen_aporte_sbif
ORDER BY mes_anno, tipo_transaccion;

PROMPT === VALIDACIÓN CASO 2 (DETALLE) ===
SELECT *
FROM detalle_aporte_sbif
ORDER BY fecha_transaccion, numrun;