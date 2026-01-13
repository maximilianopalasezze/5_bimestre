/* ============================================================
   PRACT1_PRY2206 - Actividad Formativa Semana 1 (BANK SOLUTIONS)
   ============================================================ */

SET SERVEROUTPUT ON;

/* ============================================================
   0) CONSULTAS DE APOYO (opcional)
   - Sirven para buscar RUN/DV por nombre y/o encontrar NRO_CLIENTE.
   - No son parte del bloque; puedes dejarlas como evidencia de prueba.
   ============================================================ */

-- (SQL) Buscar RUN/DV y NRO_CLIENTE por nombre (ejemplo: KAREN)
SELECT nro_cliente, numrun, dvrun,
       TRIM(pnombre || ' ' || NVL(snombre,'') || ' ' || appaterno || ' ' || NVL(apmaterno,'')) AS nombre
FROM cliente
WHERE UPPER(TRIM(pnombre || ' ' || NVL(snombre,'') || ' ' || appaterno || ' ' || NVL(apmaterno,'')))
      LIKE '%KAREN SOFIA PRADENAS MANDIOLA%';

-- (SQL) Buscar NRO_CLIENTE para los créditos de prueba del CASO 2
SELECT nro_cliente, nro_solic_credito
FROM credito_cliente
WHERE nro_solic_credito IN (2001, 3004, 2004)
ORDER BY nro_solic_credito;


/* ============================================================
   CASO 1 - Programa de Pesos TODOSUMA (SBIF)
   ============================================================ */
/* Definición de variables BIND */


/* (BIND) RUN y DV del cliente a procesar */
VAR b_numrun NUMBER
VAR b_dvrun  VARCHAR2(1)

/* (BIND) Tramos (suma de montos del año anterior) */
VAR b_tramo1_max  NUMBER
VAR b_tramo2_min  NUMBER
VAR b_tramo2_max  NUMBER

/* (BIND) Pesos */
VAR b_peso_base NUMBER
VAR b_extra1    NUMBER
VAR b_extra2    NUMBER
VAR b_extra3    NUMBER

/* (EXEC) */
EXEC :b_tramo1_max := 1000000;
EXEC :b_tramo2_min := 1000001;
EXEC :b_tramo2_max := 3000000;

EXEC :b_peso_base := 1200;
EXEC :b_extra1    := 100;
EXEC :b_extra2    := 300;
EXEC :b_extra3    := 550;

/* ============================================================
   CASO 1 - BLOQUE PL/SQL ANÓNIMO
   ============================================================ */

DECLARE
  /* (PL/SQL) Parámetros de entrada desde BIND */
  v_numrun NUMBER       := :b_numrun;
  v_dvrun  VARCHAR2(1)  := :b_dvrun;

  v_tramo1_max NUMBER   := :b_tramo1_max;
  v_tramo2_min NUMBER   := :b_tramo2_min;
  v_tramo2_max NUMBER   := :b_tramo2_max;

  v_pesos_base NUMBER   := :b_peso_base;
  v_extra1     NUMBER   := :b_extra1;
  v_extra2     NUMBER   := :b_extra2;
  v_extra3     NUMBER   := :b_extra3;

  /* (PL/SQL) Variables de salida / trabajo */
  v_nro_cliente     CLIENTE.NRO_CLIENTE%TYPE;
  v_run_cliente     VARCHAR2(15);
  v_nombre_cliente  VARCHAR2(50);
  v_tipo_cliente    VARCHAR2(30);

  v_anio_anterior   NUMBER;
  v_suma_montos     NUMBER := 0;

  v_pesos_extra     NUMBER := 0;
  v_pesos_total     NUMBER := 0;

BEGIN
  /* (PL/SQL) 1) Año anterior dinámico */
  v_anio_anterior := EXTRACT(YEAR FROM ADD_MONTHS(SYSDATE, -12));

  /* (SQL) 2) Obtener datos del cliente por RUN y DV */
  SELECT c.nro_cliente,
         TO_CHAR(c.numrun) || '-' || c.dvrun,
         TRIM(c.pnombre || ' ' || NVL(c.snombre,'') || ' ' || c.appaterno || ' ' || NVL(c.apmaterno,'')),
         tc.nombre_tipo_cliente
    INTO v_nro_cliente, v_run_cliente, v_nombre_cliente, v_tipo_cliente
  FROM cliente c
  JOIN tipo_cliente tc
    ON tc.cod_tipo_cliente = c.cod_tipo_cliente
  WHERE c.numrun = v_numrun
    AND c.dvrun  = v_dvrun;

  /* (SQL) 3) Sumar montos solicitados del año anterior (para tramo) */
  SELECT NVL(SUM(monto_solicitado), 0)
    INTO v_suma_montos
  FROM credito_cliente
  WHERE nro_cliente = v_nro_cliente
    AND EXTRACT(YEAR FROM fecha_otorga_cred) = v_anio_anterior;

  /* (PL/SQL) 4) Definir pesos extra SOLO si es trabajador independiente */
  IF UPPER(v_tipo_cliente) = UPPER('Trabajadores independientes') THEN
    IF v_suma_montos < v_tramo1_max THEN
      v_pesos_extra := v_extra1;
    ELSIF v_suma_montos BETWEEN v_tramo2_min AND v_tramo2_max THEN
      v_pesos_extra := v_extra2;
    ELSE
      v_pesos_extra := v_extra3;
    END IF;
  ELSE
    v_pesos_extra := 0;
  END IF;

  /* (PL/SQL) 5) Calcular pesos totales recorriendo los créditos del año anterior */
  v_pesos_total := 0;

  FOR r IN (
    /* (SQL) Créditos del cliente del año anterior (solo monto solicitado) */
    SELECT monto_solicitado
    FROM credito_cliente
    WHERE nro_cliente = v_nro_cliente
      AND EXTRACT(YEAR FROM fecha_otorga_cred) = v_anio_anterior
  ) LOOP
    /* (PL/SQL) Pesos por crédito: TRUNC(monto/100000) * (base + extra) */
    v_pesos_total :=
      v_pesos_total + ( TRUNC(r.monto_solicitado / 100000) * (v_pesos_base + v_pesos_extra) );
  END LOOP;

  /* (SQL) 6) Si ya existe registro, eliminarlo (según pauta para re-ejecución) */
  DELETE FROM cliente_todosuma
   WHERE nro_cliente = v_nro_cliente;

  /* (SQL) 7) Insertar resultado en CLIENTE_TODOSUMA */
  INSERT INTO cliente_todosuma
    (nro_cliente, run_cliente, nombre_cliente, tipo_cliente, monto_solic_creditos, monto_pesos_todosuma)
  VALUES
    (v_nro_cliente, v_run_cliente, v_nombre_cliente, v_tipo_cliente, v_suma_montos, v_pesos_total);

  COMMIT;

  /* (PL/SQL) Mensaje de salida */
  DBMS_OUTPUT.PUT_LINE('OK CASO 1 -> ' || v_nombre_cliente ||
                       ' | Año anterior: ' || v_anio_anterior ||
                       ' | Suma montos: ' || v_suma_montos ||
                       ' | Pesos: ' || v_pesos_total);

EXCEPTION
  WHEN NO_DATA_FOUND THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR CASO 1: RUN/DV no encontrado.');
  WHEN OTHERS THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR CASO 1: ' || SQLERRM);
END;
/

-- (SQL) Validación CASO 1
SELECT *
FROM cliente_todosuma
ORDER BY nro_cliente;

/* ============================================================
   CASO 2 - Postergación de cuotas
   ============================================================ */


VAR b_nro_cliente       NUMBER
VAR b_nro_solic_credito NUMBER
VAR b_cant_postergar    NUMBER


DECLARE
  /* (PL/SQL) Parámetros desde BIND */
  v_nro_cliente       NUMBER := :b_nro_cliente;
  v_nro_solic_credito NUMBER := :b_nro_solic_credito;
  v_cant_postergar    NUMBER := :b_cant_postergar;

  /* (PL/SQL) Datos del tipo de crédito */
  v_nombre_credito    CREDITO.NOMBRE_CREDITO%TYPE;

  /* (PL/SQL) Datos de última cuota */
  v_ult_nro_cuota      CUOTA_CREDITO_CLIENTE.NRO_CUOTA%TYPE;
  v_ult_fecha_venc     CUOTA_CREDITO_CLIENTE.FECHA_VENC_CUOTA%TYPE;
  v_ult_valor_cuota    CUOTA_CREDITO_CLIENTE.VALOR_CUOTA%TYPE;

  /* (PL/SQL) Reglas */
  v_tasa_interes       NUMBER := 0;
  v_anio_anterior      NUMBER;
  v_cant_creditos_anio NUMBER := 0;

  /* (PL/SQL) Nuevas cuotas */
  v_nueva_cuota       NUMBER;
  v_nueva_fecha_venc  DATE;
  v_nuevo_valor       NUMBER;

BEGIN
  /* (PL/SQL) 1) Año anterior dinámico */
  v_anio_anterior := EXTRACT(YEAR FROM ADD_MONTHS(SYSDATE, -12));

  /* (SQL) 2) Obtener nombre del tipo de crédito del cliente */
  SELECT c.nombre_credito
    INTO v_nombre_credito
  FROM credito_cliente cc
  JOIN credito c
    ON c.cod_credito = cc.cod_credito
  WHERE cc.nro_cliente = v_nro_cliente
    AND cc.nro_solic_credito = v_nro_solic_credito;

  /* (SQL) 3) Obtener NRO_CUOTA máximo  */
  SELECT MAX(nro_cuota)
    INTO v_ult_nro_cuota
  FROM cuota_credito_cliente
  WHERE nro_solic_credito = v_nro_solic_credito;

  /* (SQL) 4) Obtener fecha y valor de la última cuota */
  SELECT fecha_venc_cuota, valor_cuota
    INTO v_ult_fecha_venc, v_ult_valor_cuota
  FROM cuota_credito_cliente
  WHERE nro_solic_credito = v_nro_solic_credito
    AND nro_cuota = v_ult_nro_cuota;

  /* (PL/SQL) 5) Determinar tasa según reglas  */
  IF UPPER(v_nombre_credito) LIKE '%HIPOTEC%' THEN
    IF v_cant_postergar = 1 THEN
      v_tasa_interes := 0;
    ELSIF v_cant_postergar = 2 THEN
      v_tasa_interes := 0.005; -- 0,5%
    ELSE
      RAISE_APPLICATION_ERROR(-20001, 'Hipotecario: solo permite 1 o 2 cuotas.');
    END IF;

  ELSIF UPPER(v_nombre_credito) LIKE '%CONSUM%' THEN
    IF v_cant_postergar = 1 THEN
      v_tasa_interes := 0.01; -- 1%
    ELSE
      RAISE_APPLICATION_ERROR(-20002, 'Consumo: solo permite 1 cuota.');
    END IF;

  ELSIF UPPER(v_nombre_credito) LIKE '%AUTOMOT%' THEN
    IF v_cant_postergar = 1 THEN
      v_tasa_interes := 0.02; -- 2%
    ELSE
      RAISE_APPLICATION_ERROR(-20003, 'Automotriz: solo permite 1 cuota.');
    END IF;

  ELSE
    RAISE_APPLICATION_ERROR(-20004, 'Tipo de crédito no reconocido: ' || v_nombre_credito);
  END IF;

  /* (SQL) 6) Contar créditos del cliente del año anterior */
  SELECT COUNT(*)
    INTO v_cant_creditos_anio
  FROM credito_cliente
  WHERE nro_cliente = v_nro_cliente
    AND EXTRACT(YEAR FROM fecha_otorga_cred) = v_anio_anterior;

  /* (PL/SQL + SQL) 7) Si tuvo más de 1 crédito, marcar última cuota original como pagada */
  IF v_cant_creditos_anio > 1 THEN
    UPDATE cuota_credito_cliente
       SET fecha_pago_cuota = fecha_venc_cuota,
           monto_pagado     = valor_cuota
     WHERE nro_solic_credito = v_nro_solic_credito
       AND nro_cuota = v_ult_nro_cuota;
  END IF;

  /* (PL/SQL) 8) Insertar cuotas nuevas al final (FOR) */
  FOR i IN 1..v_cant_postergar LOOP
    v_nueva_cuota      := v_ult_nro_cuota + i;
    v_nueva_fecha_venc := ADD_MONTHS(v_ult_fecha_venc, i);

    /* (PL/SQL) Valor con interés según tasa definida */
    v_nuevo_valor := ROUND(v_ult_valor_cuota * (1 + v_tasa_interes));

    /* (SQL) Insertar nueva cuota con campos de pago en NULL */
    INSERT INTO cuota_credito_cliente
      (nro_solic_credito, nro_cuota, fecha_venc_cuota, valor_cuota,
       fecha_pago_cuota, monto_pagado, saldo_por_pagar, cod_forma_pago)
    VALUES
      (v_nro_solic_credito, v_nueva_cuota, v_nueva_fecha_venc, v_nuevo_valor,
       NULL, NULL, NULL, NULL);
  END LOOP;

  COMMIT;

  DBMS_OUTPUT.PUT_LINE('OK CASO 2 -> Crédito: ' || v_nro_solic_credito ||
                       ' | Tipo: ' || v_nombre_credito ||
                       ' | Postergadas: ' || v_cant_postergar);

EXCEPTION
  WHEN NO_DATA_FOUND THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR CASO 2: No se encontró cliente/crédito con esos datos.');
  WHEN OTHERS THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR CASO 2: ' || SQLERRM);
END;
/

-- (SQL) Validación CASO 2 (ajusta IN según tus pruebas)
SELECT *
FROM cuota_credito_cliente
WHERE nro_solic_credito IN (2001, 3004, 2004)
ORDER BY nro_solic_credito, nro_cuota;
