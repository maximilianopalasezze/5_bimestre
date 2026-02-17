/* =====================================================================
   ACTIVIDAD PRACT6 - PRY2206
   TEMA: Procedimientos y funciones almacenadas (PL/SQL)
   CASO: Departamentos con pago cero + actualización de multa en GASTO_COMUN

   OBJETIVO GENERAL:
   1) Generar la información de deudores (pago cero) en la tabla
      GASTO_COMUN_PAGO_CERO para el período procesado.
   2) Actualizar el valor de MULTA_GC en la tabla GASTO_COMUN para el
      período procesado, de acuerdo a reglas de negocio.
   3) Todo usando fechas dinámicas (NO fechas fijas) y parámetros.

   ===================================================================== */


/* =====================================================================
   PROCEDIMIENTO 1: PR_INS_GC_PAGO_CERO
   OBJETIVO:
   Insertar una fila en la tabla GASTO_COMUN_PAGO_CERO.
   ===================================================================== */
CREATE OR REPLACE PROCEDURE PR_INS_GC_PAGO_CERO (
    p_anno_mes_pcgc              IN     NUMBER,
    p_id_edif                    IN     NUMBER,
    p_nombre_edif                IN     VARCHAR2,
    p_run_administrador          IN     VARCHAR2,
    p_nombre_administrador       IN     VARCHAR2,
    p_nro_depto                  IN     NUMBER,
    p_run_responsable_pago_gc    IN     VARCHAR2,
    p_nombre_responsable_pago_gc IN     VARCHAR2,
    p_valor_multa_pago_cero      IN     NUMBER,
    p_observacion                IN OUT VARCHAR2,   -- IN OUT: normalizamos texto
    o_filas_insertadas           OUT    NUMBER,     -- OUT: filas insertadas
    o_mensaje                    OUT    VARCHAR2    -- OUT: mensaje de salida
) AS
BEGIN
  
    p_observacion := TRIM(p_observacion);

    /* -------------------------------------------------------------
       INSERT en tabla destino
       ------------------------------------------------------------- */
    INSERT INTO GASTO_COMUN_PAGO_CERO (
        anno_mes_pcgc,
        id_edif,
        nombre_edif,
        run_administrador,
        nombre_admnistrador,
        nro_depto,
        run_responsable_pago_gc,
        nombre_responsable_pago_gc,
        valor_multa_pago_cero,
        observacion
    )
    VALUES (
        p_anno_mes_pcgc,
        p_id_edif,
        p_nombre_edif,
        p_run_administrador,
        p_nombre_administrador,
        p_nro_depto,
        p_run_responsable_pago_gc,
        p_nombre_responsable_pago_gc,
        p_valor_multa_pago_cero,
        p_observacion
    );

    /* -------------------------------------------------------------
       Salidas OUT.
       ------------------------------------------------------------- */
    o_filas_insertadas := SQL%ROWCOUNT;
    o_mensaje := 'OK: Insert realizado en GASTO_COMUN_PAGO_CERO';

EXCEPTION
    /* -------------------------------------------------------------
       Excepción predefinida: violación de llave única (si existiera).
       ------------------------------------------------------------- */
    WHEN DUP_VAL_ON_INDEX THEN
        o_filas_insertadas := 0;
        o_mensaje := 'ERROR: Registro duplicado (DUP_VAL_ON_INDEX)';

    /* -------------------------------------------------------------
       Cualquier otra excepción inesperada.
       ------------------------------------------------------------- */
    WHEN OTHERS THEN
        o_filas_insertadas := 0;
        o_mensaje := 'ERROR: Fallo en PR_INS_GC_PAGO_CERO -> ' || SQLERRM;
END;
/
SHOW ERRORS;


/* =====================================================================
   PROCEDIMIENTO 2: PR_GEN_DEUDORES_GC
   OBJETIVO:
   - Generar deudores (pago cero) para el período procesado.
   - Actualizar MULTA_GC en GASTO_COMUN para el período procesado.
   - Considera pagos del mes anterior y dos meses anteriores.
   ===================================================================== */
CREATE OR REPLACE PROCEDURE PR_GEN_DEUDORES_GC (
    p_anno_mes_proc      IN OUT NUMBER,   
    p_valor_uf           IN     NUMBER,  
    o_total_deudores     OUT    NUMBER,  
    o_total_multas_upd   OUT    NUMBER,   
    o_mensaje            OUT    VARCHAR2  
) AS
    /* -------------------------------------------------------------
       Variables de períodos anteriores (dinámicos).
       v_prev1: mes anterior al período procesado
       v_prev2: dos meses anteriores al período procesado
       ------------------------------------------------------------- */
    v_prev1 NUMBER(6);
    v_prev2 NUMBER(6);

    /* -------------------------------------------------------------
       Variables de control / contadores (para parámetros OUT).
       ------------------------------------------------------------- */
    v_ins_count   NUMBER := 0;
    v_upd_count   NUMBER := 0;

    /* -------------------------------------------------------------
       Excepciones personalizadas (requisito rúbrica).
       ------------------------------------------------------------- */
    e_uf_invalida      EXCEPTION;
    e_periodo_invalido EXCEPTION;

    /* -------------------------------------------------------------
       Función: Formatear RUN con puntos y guión (ej: 12.345.678-9)
       ------------------------------------------------------------- */
    FUNCTION FN_RUN_FMT(p_num NUMBER, p_dv VARCHAR2) RETURN VARCHAR2 IS
        v_run VARCHAR2(30);
    BEGIN
        /* Formato con separador de miles y reemplazo a puntos (config regional) */
        v_run := REPLACE(TO_CHAR(p_num, 'FM999G999G999G999'), ',', '.') || '-' || p_dv;
        RETURN v_run;
    END FN_RUN_FMT;

    /* -------------------------------------------------------------
       Función: Armar nombre completo (maneja NULLs y espacios).
       ------------------------------------------------------------- */
    FUNCTION FN_NOMBRE_COMPLETO(p_pn VARCHAR2, p_sn VARCHAR2, p_ap VARCHAR2, p_am VARCHAR2) RETURN VARCHAR2 IS
    BEGIN
        RETURN TRIM(
               NVL(p_pn,'') || ' ' ||
               NVL(p_sn,'') || ' ' ||
               NVL(p_ap,'') || ' ' ||
               NVL(p_am,'')
        );
    END FN_NOMBRE_COMPLETO;

BEGIN
    /* =============================================================
       0) Validaciones iniciales 
       ============================================================= */

    /* Validación UF: debe ser mayor a 0 */
    IF p_valor_uf IS NULL OR p_valor_uf <= 0 THEN
        RAISE e_uf_invalida;
    END IF;

    /* Validación período: se espera formato YYYYMM (6 dígitos) */
    IF p_anno_mes_proc IS NULL OR LENGTH(TO_CHAR(p_anno_mes_proc)) <> 6 THEN
        RAISE e_periodo_invalido;
    END IF;

    /* Normalización IN OUT: asegura que quede como número YYYYMM */
    p_anno_mes_proc := TO_NUMBER(TO_CHAR(p_anno_mes_proc));

    /* =============================================================
       1) Cálculo de períodos dinámicos (NO fechas fijas)
          - Se construye fecha 01 del período y se retrocede meses.
       ============================================================= */
    v_prev1 := TO_NUMBER(TO_CHAR(ADD_MONTHS(TO_DATE(p_anno_mes_proc || '01','YYYYMMDD'), -1), 'YYYYMM'));
    v_prev2 := TO_NUMBER(TO_CHAR(ADD_MONTHS(TO_DATE(p_anno_mes_proc || '01','YYYYMMDD'), -2), 'YYYYMM'));

    /* =============================================================
       2) Limpieza para re-ejecución de pruebas
          - Elimina solo el período que se está procesando.
       ============================================================= */
    DELETE FROM GASTO_COMUN_PAGO_CERO
     WHERE anno_mes_pcgc = p_anno_mes_proc;

    /* =============================================================
       3) Cursor optimizado:
          - Se recorre GASTO_COMUN del período actual.
          - Se “marca” si existe pago en prev1 y prev2 con LEFT JOIN.
          - Evita ejecutar SELECT COUNT(*) por cada fila (optimización).
       ============================================================= */
    FOR r IN (
        SELECT
            gc.anno_mes_pcgc,
            gc.id_edif,
            e.nombre_edif,
            gc.nro_depto,
            gc.fecha_pago_gc,

            a.numrun_adm,
            a.dvrun_adm,
            a.pnombre_adm,
            a.snombre_adm,
            a.appaterno_adm,
            a.apmaterno_adm,

            rp.numrun_rpgc,
            rp.dvrun_rpgc,
            rp.pnombre_rpgc,
            rp.snombre_rpgc,
            rp.appaterno_rpgc,
            rp.apmaterno_rpgc,

            /* Flags de pago (1 = pagó / 0 = no pagó) */
            CASE WHEN p1.id_edif IS NOT NULL THEN 1 ELSE 0 END AS pago_prev1,
            CASE WHEN p2.id_edif IS NOT NULL THEN 1 ELSE 0 END AS pago_prev2

        FROM gasto_comun gc
        JOIN edificio e
          ON e.id_edif = gc.id_edif
        JOIN administrador a
          ON a.numrun_adm = e.numrun_adm
        JOIN responsable_pago_gasto_comun rp
          ON rp.numrun_rpgc = gc.numrun_rpgc

        /* Pago mes anterior */
        LEFT JOIN (
            SELECT DISTINCT anno_mes_pcgc, id_edif, nro_depto
            FROM pago_gasto_comun
            WHERE anno_mes_pcgc = v_prev1
        ) p1
          ON p1.id_edif = gc.id_edif
         AND p1.nro_depto = gc.nro_depto
         AND p1.anno_mes_pcgc = v_prev1

        /* Pago dos meses anteriores */
        LEFT JOIN (
            SELECT DISTINCT anno_mes_pcgc, id_edif, nro_depto
            FROM pago_gasto_comun
            WHERE anno_mes_pcgc = v_prev2
        ) p2
          ON p2.id_edif = gc.id_edif
         AND p2.nro_depto = gc.nro_depto
         AND p2.anno_mes_pcgc = v_prev2

        WHERE gc.anno_mes_pcgc = p_anno_mes_proc
    ) LOOP

        /* =========================================================
           3.1) Declaración de variables por iteración
           ========================================================= */
        DECLARE
            v_multa   NUMBER := 0;        -- valor multa en pesos
            v_obs     VARCHAR2(120);     

            v_run_adm VARCHAR2(30);
            v_nom_adm VARCHAR2(80);

            v_run_rp  VARCHAR2(30);
            v_nom_rp  VARCHAR2(80);

            v_obs_inout VARCHAR2(120);   
            v_filas_ins NUMBER := 0;      
            v_msg_ins   VARCHAR2(200);    
        BEGIN
            /* =====================================================
               Condición principal:
               - “Pago cero” = NO existe pago en el mes anterior (prev1)
               ===================================================== */
            IF r.pago_prev1 = 0 THEN

                /* Formateo de datos de salida solicitados */
                v_run_adm := FN_RUN_FMT(r.numrun_adm, r.dvrun_adm);
                v_nom_adm := FN_NOMBRE_COMPLETO(r.pnombre_adm, r.snombre_adm, r.appaterno_adm, r.apmaterno_adm);

                v_run_rp  := FN_RUN_FMT(r.numrun_rpgc, r.dvrun_rpgc);
                v_nom_rp  := FN_NOMBRE_COMPLETO(r.pnombre_rpgc, r.snombre_rpgc, r.appaterno_rpgc, r.apmaterno_rpgc);

          
                IF r.pago_prev2 = 0 THEN
                    /* -------------------------------------------------
                       Más de un período sin pago:
                       ------------------------------------------------- */
                    v_multa := 2 * p_valor_uf;

                    v_obs   := 'Se realizará el corte del combustible y agua a contar del '
                               || TO_CHAR(r.fecha_pago_gc, 'DD/MM/YYYY');
                ELSE
                    /* -------------------------------------------------
                       Solo 1 período sin pago:
                       ------------------------------------------------- */
                    v_multa := 1 * p_valor_uf;

                    v_obs   := 'Se realizará el corte del combustible y agua';
                END IF;

                /* =================================================
                    Insert en GASTO_COMUN_PAGO_CE
                   ================================================= */
                v_obs_inout := v_obs; 

                PR_INS_GC_PAGO_CERO(
                    p_anno_mes_pcgc              => r.anno_mes_pcgc,
                    p_id_edif                    => r.id_edif,
                    p_nombre_edif                => r.nombre_edif,
                    p_run_administrador          => v_run_adm,
                    p_nombre_administrador       => v_nom_adm,
                    p_nro_depto                  => r.nro_depto,
                    p_run_responsable_pago_gc    => v_run_rp,
                    p_nombre_responsable_pago_gc => v_nom_rp,
                    p_valor_multa_pago_cero      => v_multa,
                    p_observacion                => v_obs_inout, 
                    o_filas_insertadas           => v_filas_ins,  
                    o_mensaje                    => v_msg_ins    
                );

                /* Contador de inserts */
                v_ins_count := v_ins_count + v_filas_ins;

                /* =================================================
                     Update de MULTA_GC en GASTO_COMUN
                   - Se actualiza la multa SOLO para el período actual.
                   ================================================= */
                UPDATE gasto_comun
                   SET multa_gc = v_multa
                 WHERE anno_mes_pcgc = r.anno_mes_pcgc
                   AND id_edif       = r.id_edif
                   AND nro_depto     = r.nro_depto;

                
                v_upd_count := v_upd_count + SQL%ROWCOUNT;

            END IF;

        EXCEPTION
            /* -----------------------------------------------------
               Excepciones predefinidas dentro del LOOP:
               Si una iteración falla, se aborta el proceso completo
               ----------------------------------------------------- */
            WHEN OTHERS THEN
                RAISE; -- Propaga al manejador general para ROLLBACK
        END;

    END LOOP;

    /* =============================================================
       4) Confirmación de transacción
       ============================================================= */
    COMMIT;

 
    o_total_deudores   := v_ins_count;
    o_total_multas_upd := v_upd_count;
    o_mensaje := 'OK: Proceso ejecutado. Deudores=' || o_total_deudores ||
                 ', Multas actualizadas=' || o_total_multas_upd ||
                 ', Período=' || p_anno_mes_proc ||
                 ', UF=' || p_valor_uf;

EXCEPTION

    WHEN e_uf_invalida THEN
        ROLLBACK;
        o_total_deudores := 0;
        o_total_multas_upd := 0;
        o_mensaje := 'ERROR: UF inválida. Debe ser mayor a 0.';

    WHEN e_periodo_invalido THEN
        ROLLBACK;
        o_total_deudores := 0;
        o_total_multas_upd := 0;
        o_mensaje := 'ERROR: Período inválido. Debe venir en formato YYYYMM (6 dígitos).';

    WHEN NO_DATA_FOUND THEN
        /* NO_DATA_FOUND puede ocurrir en algunas consultas si existieran SELECT INTO.
           En este procedimiento principal no usamos SELECT INTO crítico, pero se deja
           por cumplimiento de rúbrica (excepción predefinida). */
        ROLLBACK;
        o_total_deudores := 0;
        o_total_multas_upd := 0;
        o_mensaje := 'ERROR: NO_DATA_FOUND (no se encontró información esperada).';

    WHEN DUP_VAL_ON_INDEX THEN
        ROLLBACK;
        o_total_deudores := 0;
        o_total_multas_upd := 0;
        o_mensaje := 'ERROR: DUP_VAL_ON_INDEX (registro duplicado inesperado).';

    WHEN OTHERS THEN
        ROLLBACK;
        o_total_deudores := 0;
        o_total_multas_upd := 0;
        o_mensaje := 'ERROR GENERAL en PR_GEN_DEUDORES_GC -> ' || SQLERRM;
END;
/
SHOW ERRORS;


/* =====================================================================
   BLOQUE DE PRUEBA 
   - Simular ejecución para MAYO del año actual
   - Valor UF = 29.509
   ===================================================================== */
SET SERVEROUTPUT ON;

DECLARE
    v_mayo          NUMBER(6);
    v_total_deud    NUMBER;
    v_total_upd     NUMBER;
    v_msg           VARCHAR2(400);
BEGIN
    /* -------------------------------------------------------------
       Cálculo dinámico de Mayo del año actual:
       - TRUNC(SYSDATE,'YYYY') = 01/01 del año actual
       - ADD_MONTHS(...,4) = Mayo
       ------------------------------------------------------------- */
    v_mayo := TO_NUMBER(TO_CHAR(ADD_MONTHS(TRUNC(SYSDATE, 'YYYY'), 4), 'YYYYMM'));

    /* Ejecución del proceso principal */
    PR_GEN_DEUDORES_GC(
        p_anno_mes_proc    => v_mayo,     -- IN OUT
        p_valor_uf         => 29509,      -- IN
        o_total_deudores   => v_total_deud, -- OUT
        o_total_multas_upd => v_total_upd,  -- OUT
        o_mensaje          => v_msg         -- OUT
    );

    /* Impresión de salidas */
    DBMS_OUTPUT.PUT_LINE('Resultado: ' || v_msg);
END;
/
 

/* =====================================================================
   SELECTS DE EVIDENCIA 
   ===================================================================== */

-- Figura 1 
SELECT
    anno_mes_pcgc,
    id_edif,
    nombre_edif,
    run_administrador,
    nombre_admnistrador,
    nro_depto,
    run_responsable_pago_gc,
    nombre_responsable_pago_gc
FROM gasto_comun_pago_cero
ORDER BY nombre_edif ASC, nro_depto ASC;

-- Observación aparte 
SELECT
    observacion
FROM gasto_comun_pago_cero
ORDER BY nombre_edif ASC, nro_depto ASC;

-- Figura 2: Multas en GASTO_COMUN para el período procesado
SELECT
    anno_mes_pcgc,
    id_edif,
    nro_depto,
    fecha_desde_gc,
    fecha_hasta_gc,
    multa_gc
FROM gasto_comun
WHERE anno_mes_pcgc = TO_NUMBER(TO_CHAR(ADD_MONTHS(TRUNC(SYSDATE, 'YYYY'), 4), 'YYYYMM'))
  AND NVL(multa_gc, 0) > 0
ORDER BY id_edif ASC, nro_depto ASC;