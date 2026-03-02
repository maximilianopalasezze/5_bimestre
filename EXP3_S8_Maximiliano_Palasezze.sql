/* =========================================================
  
   - Caso 1: Trigger para mantener TOTAL_CONSUMOS ante I/U/D en CONSUMO
   - Caso 2: Package + Funciones + Procedimiento principal
   - Manejo de errores: REG_ERRORES con SQ_ERROR
   - Resultados: DETALLE_DIARIO_HUESPEDES
   ========================================================= */


SET SERVEROUTPUT ON;

/* =========================================================
    LIMPIEZA DE OBJETOS 
   ========================================================= */

BEGIN
  EXECUTE IMMEDIATE 'DROP TRIGGER TRG_TOTAL_CONSUMOS';
EXCEPTION WHEN OTHERS THEN NULL;
END;
/

BEGIN
  EXECUTE IMMEDIATE 'DROP PACKAGE PK_TOURS';
EXCEPTION WHEN OTHERS THEN NULL;
END;
/

BEGIN
  EXECUTE IMMEDIATE 'DROP PROCEDURE SP_LOG_ERROR';
EXCEPTION WHEN OTHERS THEN NULL;
END;
/

BEGIN
  EXECUTE IMMEDIATE 'DROP FUNCTION FN_AGENCIA';
EXCEPTION WHEN OTHERS THEN NULL;
END;
/

BEGIN
  EXECUTE IMMEDIATE 'DROP FUNCTION FN_CONSUMOS';
EXCEPTION WHEN OTHERS THEN NULL;
END;
/

BEGIN
  EXECUTE IMMEDIATE 'DROP PROCEDURE SP_DETALLE_DIARIO';
EXCEPTION WHEN OTHERS THEN NULL;
END;
/

/* =========================================================
    PROCEDIMIENTO UTILITARIO PARA LOGUEAR ERRORES (REG_ERRORES)
   ========================================================= */
CREATE OR REPLACE PROCEDURE SP_LOG_ERROR
(
  p_nomsubprograma IN VARCHAR2,
  p_msg_error      IN VARCHAR2
)
IS
BEGIN
  INSERT INTO REG_ERRORES (ID_ERROR, NOMSUBPROGRAMA, MSG_ERROR)
  VALUES (SQ_ERROR.NEXTVAL, p_nomsubprograma, p_msg_error);
EXCEPTION
  WHEN OTHERS THEN
    -- Si incluso fallara el log, no se debe caer todo el proceso
    NULL;
END;
/
SHOW ERRORS;

/* =========================================================
    CASO 1 - TRIGGER
   - Mantiene TOTAL_CONSUMOS cuando se INSERT/UPDATE/DELETE en CONSUMO
   ========================================================= */
CREATE OR REPLACE TRIGGER TRG_TOTAL_CONSUMOS
AFTER INSERT OR UPDATE OR DELETE ON CONSUMO
FOR EACH ROW
DECLARE
  v_existe NUMBER;
BEGIN
  /* ========================
      SUMAR MONTO
     ======================== */
  IF INSERTING THEN
    SELECT COUNT(*)
    INTO v_existe
    FROM TOTAL_CONSUMOS
    WHERE ID_HUESPED = :NEW.ID_HUESPED;

    IF v_existe = 0 THEN
      INSERT INTO TOTAL_CONSUMOS (ID_HUESPED, MONTO_CONSUMOS)
      VALUES (:NEW.ID_HUESPED, :NEW.MONTO);
    ELSE
      UPDATE TOTAL_CONSUMOS
      SET MONTO_CONSUMOS = MONTO_CONSUMOS + :NEW.MONTO
      WHERE ID_HUESPED = :NEW.ID_HUESPED;
    END IF;

  /* ========================
     RESTAR MONTO
     ======================== */
  ELSIF DELETING THEN
    UPDATE TOTAL_CONSUMOS
    SET MONTO_CONSUMOS = MONTO_CONSUMOS - :OLD.MONTO
    WHERE ID_HUESPED = :OLD.ID_HUESPED;

  /* ========================
      AJUSTAR DIFERENCIA
     ======================== */
  ELSIF UPDATING THEN
    UPDATE TOTAL_CONSUMOS
    SET MONTO_CONSUMOS = MONTO_CONSUMOS + (:NEW.MONTO - :OLD.MONTO)
    WHERE ID_HUESPED = :NEW.ID_HUESPED;
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    -- Si algo raro pasa, lo registramos 
    SP_LOG_ERROR('TRG_TOTAL_CONSUMOS', SQLERRM);
END;
/
SHOW ERRORS;

/* =========================================================
   CASO 2- PACKAGE PARA TOURS
   - Función devuelve monto USD de tours
   - Si no hay tours -> devuelve 0
   - Incluye variable global para reutilizar el valor
   ========================================================= */
CREATE OR REPLACE PACKAGE PK_TOURS AS
  g_monto_tours NUMBER := 0;

  FUNCTION FN_MONTO_TOURS(p_id_huesped NUMBER) RETURN NUMBER;
END PK_TOURS;
/
SHOW ERRORS;

CREATE OR REPLACE PACKAGE BODY PK_TOURS AS
  FUNCTION FN_MONTO_TOURS(p_id_huesped NUMBER) RETURN NUMBER IS
    v_total NUMBER;
  BEGIN
    /* ======================================================
       Monto Tours (USD):
       HUESPED_TOUR guarda tours del huésped y NUM_PERSONAS.
       TOUR guarda VALOR_TOUR.
       ====================================================== */
    SELECT NVL(SUM(t.VALOR_TOUR * ht.NUM_PERSONAS), 0)
    INTO v_total
    FROM HUESPED_TOUR ht
    JOIN TOUR t ON t.ID_TOUR = ht.ID_TOUR
    WHERE ht.ID_HUESPED = p_id_huesped;

    g_monto_tours := v_total;
    RETURN v_total;

  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      g_monto_tours := 0;
      RETURN 0;
    WHEN OTHERS THEN
      SP_LOG_ERROR('PK_TOURS.FN_MONTO_TOURS - ID_HUESPED='||p_id_huesped, SQLERRM);
      g_monto_tours := 0;
      RETURN 0;
  END;
END PK_TOURS;
/
SHOW ERRORS;

/* =========================================================
   CASO 2- FUNCIONES ALMACENADAS
   A) FN_AGENCIA: retorna agencia del huésped.
      - Controla errores guardando en REG_ERRORES 
      - Retorna "NO REGISTRA AGENCIA" si falla.
   B) FN_CONSUMOS: retorna monto USD desde TOTAL_CONSUMOS.
      - Si no tiene consumos -> 0 
   ========================================================= */

CREATE OR REPLACE FUNCTION FN_AGENCIA(p_id_huesped NUMBER)
RETURN VARCHAR2
IS
  v_agencia AGENCIA.NOM_AGENCIA%TYPE;
BEGIN
  SELECT a.NOM_AGENCIA
  INTO v_agencia
  FROM HUESPED h
  JOIN AGENCIA a ON a.ID_AGENCIA = h.ID_AGENCIA
  WHERE h.ID_HUESPED = p_id_huesped;

  RETURN v_agencia;

EXCEPTION
  WHEN NO_DATA_FOUND THEN
    SP_LOG_ERROR('FN_AGENCIA - ID_HUESPED='||p_id_huesped, SQLERRM);
    RETURN 'NO REGISTRA AGENCIA';
  WHEN OTHERS THEN
    SP_LOG_ERROR('FN_AGENCIA - ID_HUESPED='||p_id_huesped, SQLERRM);
    RETURN 'NO REGISTRA AGENCIA';
END;
/
SHOW ERRORS;

CREATE OR REPLACE FUNCTION FN_CONSUMOS(p_id_huesped NUMBER)
RETURN NUMBER
IS
  v_consumos NUMBER;
BEGIN
  SELECT MONTO_CONSUMOS
  INTO v_consumos
  FROM TOTAL_CONSUMOS
  WHERE ID_HUESPED = p_id_huesped;

  RETURN NVL(v_consumos, 0);

EXCEPTION
  WHEN NO_DATA_FOUND THEN
    -- Regla: si no registra consumos, debe devolver 0
    RETURN 0;
  WHEN OTHERS THEN
    -- Si ocurrió un error real (ej. tabla no existe), lo registramos
    SP_LOG_ERROR('FN_CONSUMOS - ID_HUESPED='||p_id_huesped, SQLERRM);
    RETURN 0;
END;
/
SHOW ERRORS;

/* =========================================================
    CASO 2- PROCEDIMIENTO PRINCIPAL
   - Parámetros:
     p_fecha_proceso (para la prueba: 18/08/2021)
     p_tipo_cambio   (para la prueba: 915)
   - Limpia DETALLE_DIARIO_HUESPEDES y REG_ERRORES 
   - Calcula:
     * ALOJAMIENTO (USD) = (hab + minibar) diario * estadía
     * CONSUMOS (USD) desde TOTAL_CONSUMOS
     * TOURS (USD) desde Package
     * PERSONAS: $35.000 CLP por persona -> se convierte a USD
       (Asunción: 1 persona por habitación reservada)
     * SUBTOTAL = alojamiento + consumos + tours + personas
     * DESCUENTO_CONSUMOS según TRAMOS_CONSUMOS (PCT)
     * DESCUENTO_AGENCIA 12% si agencia = "Viajes Alberti"
     * TOTAL = subtotal - descuentos
   - Guarda todo redondeado y convertido a CLP.
   ========================================================= */
CREATE OR REPLACE PROCEDURE SP_DETALLE_DIARIO
(
  p_fecha_proceso IN DATE,
  p_tipo_cambio   IN NUMBER
)
IS
  CURSOR c_huespedes IS
    SELECT r.ID_HUESPED, r.ID_RESERVA, r.ESTADIA
    FROM RESERVA r
    WHERE (r.INGRESO + r.ESTADIA) = p_fecha_proceso;

  v_nombre            VARCHAR2(200);
  v_agencia           VARCHAR2(100);

  v_aloj_usd          NUMBER;
  v_cons_usd          NUMBER;
  v_tours_usd         NUMBER;

  v_num_personas      NUMBER;  -- ASUNCIÓN: 1 persona por habitación
  v_personas_clp      NUMBER;
  v_personas_usd      NUMBER;

  v_subtotal_usd      NUMBER;

  v_pct_tramo         NUMBER;
  v_desc_cons_usd     NUMBER;

  v_desc_agencia_usd  NUMBER;
  v_total_usd         NUMBER;

BEGIN
  /* ======================================================
     LIMPIEZA PARA PODER EJECUTAR N VECES 
     ====================================================== */
  DELETE FROM DETALLE_DIARIO_HUESPEDES;
  DELETE FROM REG_ERRORES;
  COMMIT;

  /* ======================================================
     PROCESO MASIVO: huéspedes con salida en p_fecha_proceso
     ====================================================== */
  FOR x IN c_huespedes LOOP
    BEGIN
      /* ===== Nombre del huésped ===== */
      SELECT h.NOM_HUESPED || ' ' || h.APPAT_HUESPED || ' ' || h.APMAT_HUESPED
      INTO v_nombre
      FROM HUESPED h
      WHERE h.ID_HUESPED = x.ID_HUESPED;

      /* ===== Agencia (función con log de error) ===== */
      v_agencia := FN_AGENCIA(x.ID_HUESPED);

      /* ===== Alojamiento USD =====
         Sumatoria diaria por habitación reservada: VALOR_HABITACION + VALOR_MINIBAR
         Luego se multiplica por días (ESTADIA)
      */
      SELECT NVL(SUM(ha.VALOR_HABITACION + ha.VALOR_MINIBAR), 0)
      INTO v_aloj_usd
      FROM DETALLE_RESERVA dr
      JOIN HABITACION ha ON ha.ID_HABITACION = dr.ID_HABITACION
      WHERE dr.ID_RESERVA = x.ID_RESERVA;

      v_aloj_usd := v_aloj_usd * x.ESTADIA;

      /* ===== Consumos USD ===== */
      v_cons_usd := FN_CONSUMOS(x.ID_HUESPED);

      /* ===== Tours USD (Package) ===== */
      v_tours_usd := PK_TOURS.FN_MONTO_TOURS(x.ID_HUESPED);

      /* ===== Personas =====
         Regla de negocio: $35.000 CLP por persona
         MODELO NO TRAE "NUM_PERSONAS" EN RESERVA,
         por lo tanto se usa la ASUNCIÓN:
         -> 1 persona por habitación reservada
      */
      SELECT NVL(COUNT(*), 0)
      INTO v_num_personas
      FROM DETALLE_RESERVA
      WHERE ID_RESERVA = x.ID_RESERVA;

      v_personas_clp := 35000 * v_num_personas;

      -- Convertimos CLP->USD para mantener los cálculos en USD
      v_personas_usd := ROUND(v_personas_clp / p_tipo_cambio);

      /* ===== Subtotal USD ===== */
      v_subtotal_usd := ROUND(v_aloj_usd + v_cons_usd + v_tours_usd + v_personas_usd);

      /* ===== Descuento por consumos según TRAMOS_CONSUMOS ===== */
      BEGIN
        SELECT pct
        INTO v_pct_tramo
        FROM TRAMOS_CONSUMOS
        WHERE v_cons_usd BETWEEN vmin_tramo AND vmax_tramo;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          v_pct_tramo := 0;
      END;

      v_desc_cons_usd := ROUND(v_cons_usd * v_pct_tramo);

      /* ===== Descuento por agencia (12%) si Viajes Alberti ===== */
      IF UPPER(v_agencia) = 'VIAJES ALBERTI' THEN
        v_desc_agencia_usd := ROUND(v_subtotal_usd * 0.12);
      ELSE
        v_desc_agencia_usd := 0;
      END IF;

      /* ===== Total USD ===== */
      v_total_usd := ROUND(v_subtotal_usd - v_desc_cons_usd - v_desc_agencia_usd);

      /* ===== Guardar en DETALLE_DIARIO_HUESPEDES  ===== */
      INSERT INTO DETALLE_DIARIO_HUESPEDES
      (
        ID_HUESPED, NOMBRE, AGENCIA,
        ALOJAMIENTO, CONSUMOS, TOURS,
        SUBTOTAL_PAGO, DESCUENTO_CONSUMOS,
        DESCUENTOS_AGENCIA, TOTAL
      )
      VALUES
      (
        x.ID_HUESPED, v_nombre, v_agencia,
        ROUND(v_aloj_usd * p_tipo_cambio),
        ROUND(v_cons_usd * p_tipo_cambio),
        ROUND(v_tours_usd * p_tipo_cambio),
        ROUND(v_subtotal_usd * p_tipo_cambio),
        ROUND(v_desc_cons_usd * p_tipo_cambio),
        ROUND(v_desc_agencia_usd * p_tipo_cambio),
        ROUND(v_total_usd * p_tipo_cambio)
      );

    EXCEPTION
      WHEN OTHERS THEN
        -- Si un huésped falla, se registra y el proceso sigue con el siguiente
        SP_LOG_ERROR('SP_DETALLE_DIARIO - ID_HUESPED='||x.ID_HUESPED, SQLERRM);
    END;
  END LOOP;

  COMMIT;
END;
/
SHOW ERRORS;

/* =========================================================
    CASO 1 - BLOQUE DE PRUEBAS DEL TRIGGER
  
   ========================================================= */

SELECT * FROM CONSUMO WHERE ID_CONSUMO IN (11473, 10688) OR (ID_HUESPED=340006 AND ID_RESERVA=1587) ORDER BY ID_CONSUMO;
SELECT * FROM TOTAL_CONSUMOS WHERE ID_HUESPED IN (340006, 340009, 340003, 340004, 340008) ORDER BY ID_HUESPED;

BEGIN
  -- INSERT nuevo consumo
  INSERT INTO CONSUMO (ID_CONSUMO, ID_RESERVA, ID_HUESPED, MONTO)
  VALUES ((SELECT NVL(MAX(ID_CONSUMO),0) + 1 FROM CONSUMO), 1587, 340006, 150);

  -- DELETE consumo 11473
  DELETE FROM CONSUMO WHERE ID_CONSUMO = 11473;

  -- UPDATE consumo 10688 -> 95
  UPDATE CONSUMO SET MONTO = 95 WHERE ID_CONSUMO = 10688;

  COMMIT;
END;
/
-- ====== CONSULTA DESPUÉS DEL CASO 1 (para comparar con la Figura 2) ======
SELECT * FROM CONSUMO WHERE ID_CONSUMO IN (11473, 10688) OR (ID_HUESPED=340006 AND ID_RESERVA=1587) ORDER BY ID_CONSUMO;
 SELECT * FROM TOTAL_CONSUMOS WHERE ID_HUESPED IN (340006, 340009, 340003, 340004, 340008) ORDER BY ID_HUESPED;

/* =========================================================
    CASO 2 - EJECUCIÓN PROCEDIMIENTO PRINCIPAL
   - Considerar como día actual: 18/08/2021 (parámetro)
   - Tipo de cambio: 915 (parámetro)
   ========================================================= */
BEGIN
  SP_DETALLE_DIARIO(
    p_fecha_proceso => TO_DATE('18/08/2021','DD/MM/YYYY'),
    p_tipo_cambio   => 915
  );
END;
/
-- ====== CONSULTAS FINALES  ======
SELECT * FROM DETALLE_DIARIO_HUESPEDES ORDER BY ID_HUESPED;
SELECT * FROM REG_ERRORES ORDER BY ID_ERROR;