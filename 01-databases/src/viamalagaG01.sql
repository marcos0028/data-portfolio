


/*******************************************************/
/*---------------------Viamalaga-SQL-------------------*/
/*******************************************************/

/*Borramos todas la tablas*/
DROP TABLE IF EXISTS infraccion;
DROP TABLE IF EXISTS elemento_desmontable;
DROP TABLE IF EXISTS terraza;
DROP TABLE IF EXISTS toldos_y_voladizos;
DROP TABLE IF EXISTS otras_instalaciones;
DROP TABLE IF EXISTS aprovechamiento_especial;
DROP TABLE IF EXISTS titularidad;
DROP TABLE IF EXISTS establecimiento;
DROP TABLE IF EXISTS autorizacion;
DROP TABLE IF EXISTS solicitante;

/*Borramos todos los type*/
DROP TYPE IF EXISTS tipo_solicitante;
DROP TYPE IF EXISTS grado_infraccion;
DROP TYPE IF EXISTS modalidad;
DROP TYPE IF EXISTS tipo_desmontable;
DROP TYPE IF EXISTS tipo_fijo;
DROP TYPE IF EXISTS tipo_actividad;

/*Creamos los tipos que vamos a usar en los enum*/
CREATE TYPE tipo_solicitante AS ENUM ('persona_fisica', 'persona_juridica');
CREATE TYPE grado_infraccion AS ENUM ('Leve', 'Grave', 'Muy Grave');
CREATE TYPE modalidad AS ENUM ('Anual', 'Temporal', 'Ocasional');
CREATE TYPE tipo_desmontable AS ENUM ('mesa', 'silla', 'sombrilla', 'jardinera', 'cortaviento', 'celosia', 'velador', 'otro');
CREATE TYPE tipo_fijo AS ENUM ('toldo_pared',  'toldo_pavimento', 'voladizo');
CREATE TYPE tipo_actividad AS ENUM ('expositor', 'cartel_indicador', 'reclamo_publicitario', 'delimitador_acceso', 'rampa_acceso', 'actividad_divulgativa', 'vehiculo_promocional', 'rodaje');

/* Creación de las tablas*/

CREATE TABLE solicitante
(
    dni_o_cif_solicitante CHAR(9) NOT NULL,
    tipo_solicitante tipo_solicitante NOT NULL,
    hay_convenio BIT NOT NULL,
    PRIMARY KEY (dni_o_cif_solicitante)
);

CREATE TABLE autorizacion   
(
    id_autorizacion INT NOT NULL,
    dni_o_cif_solicitante CHAR(9) NOT NULL,
    fianza FLOAT NOT NULL,
    horario_inicio_autorizado TIME NOT NULL,
    horario_fin_autorizado TIME NOT NULL,
    fecha_inicio_autorizacion DATE NOT NULL,
    fecha_fin_autorizacion DATE NOT NULL,
    superficie_ocupable FLOAT NOT NULL,
    fecha_revocacion DATE,
    motivo_revocacion VARCHAR(8000),
    PRIMARY KEY (id_autorizacion),
    FOREIGN KEY (dni_o_cif_solicitante) REFERENCES solicitante (dni_o_cif_solicitante)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CHECK (id_autorizacion >= 0),
    CHECK (fianza >= 0),
    CHECK (fecha_inicio_autorizacion < fecha_fin_autorizacion),
    CHECK (superficie_ocupable >= 0),
    CHECK (horario_inicio_autorizado < horario_fin_autorizado),
    CHECK (fecha_revocacion IS NULL OR (fecha_revocacion BETWEEN fecha_inicio_autorizacion AND fecha_fin_autorizacion))
);

CREATE TABLE infraccion
(
    fecha_infraccion TIMESTAMP NOT NULL,
    grado_infraccion grado_infraccion NOT NULL,
    id_autorizacion INT NOT NULL,
    infraccion_cometida VARCHAR(8000) NOT NULL,
    cantidad_sancion FLOAT NOT NULL,
    PRIMARY KEY (fecha_infraccion, infraccion_cometida, id_autorizacion),
    FOREIGN KEY (id_autorizacion) REFERENCES autorizacion (id_autorizacion)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CHECK (cantidad_sancion >= 0),
    CHECK (fecha_infraccion <= CURRENT_TIMESTAMP),
    CHECK ((grado_infraccion='Leve' AND cantidad_sancion BETWEEN 0 AND 750) OR 
        (grado_infraccion='Grave' AND cantidad_sancion BETWEEN 751 AND 1500) OR 
        (grado_infraccion='Muy Grave' AND cantidad_sancion BETWEEN 1501 AND 3000))
);

CREATE TABLE establecimiento
(
    id_establecimiento INT NOT NULL,
    dni_o_cif_solicitante CHAR(9) NOT NULL,
    ubicacion_establecimiento VARCHAR(8000) NOT NULL,
    instalacion_electrica BIT NOT NULL,
    superficie FLOAT NOT NULL,
    plan_especial_aprovechamiento BIT NOT NULL,
    hora_apertura TIME NOT NULL,
    hora_cierre TIME NOT NULL,
    PRIMARY KEY (id_establecimiento),
    CHECK (superficie > 0),
    CHECK (hora_apertura <> hora_cierre),
    CHECK (hora_cierre > hora_apertura)

);

CREATE TABLE terraza
(
    id_autorizacion INT NOT NULL,
    id_establecimiento INT NOT NULL,
    modalidad_ocupacion modalidad NOT NULL,
    distribucion_elementos VARCHAR(8000) NOT NULL,
    espacios_privados_uso_publico BIT NOT NULL,
    PRIMARY KEY (id_autorizacion),
    FOREIGN KEY (id_autorizacion) REFERENCES autorizacion (id_autorizacion)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    FOREIGN KEY (id_establecimiento) REFERENCES establecimiento (id_establecimiento)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);


CREATE TABLE toldos_y_voladizos
(
    id_autorizacion INT NOT NULL,
    id_establecimiento INT NOT NULL,
    tipo_fijo tipo_fijo NOT NULL,
    numero_cerramientos INT,
    caracteristicas_elementos_fijos VARCHAR(8000) NOT NULL,
    numero_elementos_fijos INT NOT NULL,
    ubicacion_fijo VARCHAR(8000) NOT NULL,
    PRIMARY KEY (id_autorizacion),
    FOREIGN KEY (id_autorizacion) REFERENCES autorizacion (id_autorizacion)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    FOREIGN KEY (id_establecimiento) REFERENCES establecimiento (id_establecimiento)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CHECK ((tipo_fijo = 'voladizo' AND numero_cerramientos IS NULL) OR (tipo_fijo != 'voladizo' AND numero_cerramientos IS NOT NULL)),
    CHECK (numero_cerramientos IS NULL OR( numero_cerramientos BETWEEN 0 AND  3)),
    CHECK (numero_elementos_fijos > 0)
);



CREATE TABLE otras_instalaciones
(
    id_autorizacion INT NOT NULL,
    id_establecimiento INT NOT NULL,
    tipo_actividad tipo_actividad NOT NULL,
    descripcion_actividad VARCHAR(8000) NOT NULL,
    coste_actividad FLOAT NOT NULL,
    PRIMARY KEY (id_autorizacion),
    FOREIGN KEY (id_autorizacion) REFERENCES autorizacion (id_autorizacion)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    FOREIGN KEY (id_establecimiento) REFERENCES establecimiento (id_establecimiento)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CHECK (coste_actividad >= 0)
);


CREATE TABLE aprovechamiento_especial
(
    id_autorizacion INT NOT NULL,
    aportacion_economica_minima FLOAT NOT NULL,
    ubicacion_parcela VARCHAR(8000) NOT NULL,
    PRIMARY KEY (id_autorizacion),
    FOREIGN KEY (id_autorizacion) REFERENCES autorizacion (id_autorizacion)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CHECK (aportacion_economica_minima > 0)
);


CREATE TABLE elemento_desmontable
(
    tipo_desmontable tipo_desmontable NOT NULL,
    id_autorizacion INT NOT NULL,
    numero_elementos_desmontables INT NOT NULL,
    caracteristicas_elemento_desmontable VARCHAR(8000) NOT NULL,
    PRIMARY KEY (tipo_desmontable, id_autorizacion),
    FOREIGN KEY (id_autorizacion) REFERENCES autorizacion (id_autorizacion)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CHECK (numero_elementos_desmontables > 0)
);


CREATE TABLE titularidad
(
    dni_o_cif_solicitante CHAR(9) NOT NULL,
    id_establecimiento INT NOT NULL,
    fecha_inicio_titularidad DATE NOT NULL,
    fecha_fin_titularidad DATE,
    PRIMARY KEY (dni_o_cif_solicitante, id_establecimiento),
    FOREIGN KEY (dni_o_cif_solicitante) REFERENCES solicitante (dni_o_cif_solicitante)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    FOREIGN KEY (id_establecimiento) REFERENCES establecimiento (id_establecimiento)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CHECK (fecha_inicio_titularidad <= CURRENT_DATE),
    CHECK (fecha_fin_titularidad IS NULL OR (fecha_inicio_titularidad < fecha_fin_titularidad))
);



/******************************************************************/
/*---------------------Inserciones-de-Ejemplo---------------------*/
/******************************************************************/

/*---------------------SOLICITANTE---------------------*/

-- Inserciones a la tabla solicitante
INSERT INTO  solicitante(dni_o_cif_solicitante, tipo_solicitante, hay_convenio)
VALUES 
    ('71311220R', 'persona_fisica', '0'),
    ('A12345678', 'persona_juridica', '0'),
    ('71456892F', 'persona_fisica', '1'),
    ('C75284536', 'persona_juridica', '1'),
    ('56745678A', 'persona_fisica', '1');

/*---------------------AUTORIZACION---------------------*/

-- Inserciones a la tabla autorizacion

INSERT INTO autorizacion (id_autorizacion, dni_o_cif_solicitante, fianza, horario_inicio_autorizado, horario_fin_autorizado, fecha_inicio_autorizacion, fecha_fin_autorizacion, superficie_ocupable, fecha_revocacion, motivo_revocacion) 
VALUES
    (001, '71311220R', 500, '08:00:00', '22:00:00', '2024-01-01', '2024-12-31', 25.5, NULL, NULL),
    (002, 'A12345678', 1000, '09:00:00', '20:00:00', '2024-03-01', '2024-06-30', 15.0, '2024-05-01', 'Incumplimiento de normas'),
    (003, '71456892F', 200, '10:00:00', '23:59:59', '2024-02-15', '2024-11-15', 10.0, NULL, NULL),
    (004, '71311220R', 300, '09:00:00', '23:59:59', '2024-02-15', '2024-11-15', 10.0, NULL, NULL),
    (005, '71311220R', 600, '10:00:00', '23:59:59', '2024-02-15', '2024-11-15', 10.0, NULL, NULL),
    (006, '71311220R', 600, '10:00:00', '23:59:59', '2024-02-15', '2024-11-15', 10.0, NULL, NULL),
    (007, 'A12345678', 1000, '09:00:00', '20:00:00', '2024-03-01', '2024-06-30', 15.0, NULL, NULL),
    (008, '71456892F', 200, '10:00:00', '23:59:59', '2024-02-15', '2024-11-15', 10.0, NULL, NULL),
    (009, '71456892F', 0, '11:30:00', '22:00:00', '2024-02-15', '2024-11-15', 10.0, NULL, NULL),
    (010, '71311220R', 200, '10:00:00', '23:59:59', '2024-02-15', '2024-11-15', 10.0, NULL, NULL),
    (011, 'C75284536', 200, '13:00:00', '21:59:59', '2024-02-15', '2024-11-15', 10.0, NULL, NULL);


/*---------------------INFRACCION---------------------*/

-- Inserciones a la tabla infraccion
INSERT INTO infraccion (fecha_infraccion, grado_infraccion, infraccion_cometida, cantidad_sancion, id_autorizacion) 
VALUES
    ('2024-04-15 10:00:00', 'Leve', 'Ocupación indebida de espacio público', 500, 001),
    ('2023-04-15 10:00:00', 'Leve', 'Ocupación indebida de espacio público', 500, 001),
    ('2024-05-10 14:30:00', 'Grave', 'Incumplimiento de horario autorizado', 1000, 002),
    ('2024-06-01 18:00:00', 'Muy Grave', 'Instalación no autorizada de elementos desmontables', 2000, 003);

/*---------------------ESTABLECIMIENTO---------------------*/

-- Inserciones a la tabla establecimiento
INSERT INTO establecimiento(id_establecimiento, dni_o_cif_solicitante, ubicacion_establecimiento, instalacion_electrica, superficie, plan_especial_aprovechamiento, hora_apertura, hora_cierre)
VALUES
(1, '71311220R', 'Calle Larios 15', '0', 20.0, '0','08:00:00', '22:00:00'),
(2, 'A12345678', 'Avenida de Andalucía, 4', '1', 25.5, '1','09:00:00', '20:00:00'),
(3, '71456892F', 'Puerto de Málaga, 15', '1', 10.0, '0', '10:00:00', '23:59:00'),
(4, '71311220R', 'Calle Larios 13', '0', 20.0, '0','08:00:00', '22:00:00'),
(15, '56745678A', 'Plaza Espana, 3', '0', 30.0, '0', '11:00:00', '23:59:00');



/*---------------------TERRAZA---------------------*/

-- Inserciones a la tabla terraza
INSERT INTO terraza (id_autorizacion, id_establecimiento, modalidad_ocupacion, distribucion_elementos, espacios_privados_uso_publico)
VALUES
 (001, 1, 'Anual', 'La terraza tiene 4 mesas y 16 sillas, situadas en dos filas de elementos con 2 mesas y 8 sillas en cada fila', '1'),
 (002, 2, 'Temporal', 'La terraza tiene 4 mesas y 16 sillas. Además contiene dos sombrillas y tres cortavientos', '0'),
 (003, 3, 'Ocasional', 'La terraza tiene 2 mesas y 8 sillas. Además contiene dos sombrillas', '0');


/*---------------------TOLDOS_Y_VOLADIZOS---------------------*/

-- Inserciones a la tabla toldos_y_voladizos
INSERT INTO toldos_y_voladizos (id_autorizacion, id_establecimiento, tipo_fijo, numero_cerramientos, caracteristicas_elementos_fijos, numero_elementos_fijos, ubicacion_fijo)
VALUES
    (004, 1, 'toldo_pared', 0, 'Toldo rojo enrollable con el logo de Mahou e ignífugo', 2, 'En la entrada del establecimiento'),
    (005, 1, 'toldo_pavimento', 2, 'Toldo azul de material flexible', 1, 'En la entrada del establecimiento'),
    (006, 1, 'voladizo', NULL, 'Voladizo blanco de hormigón', 1, 'Zona frontal del restaurante');


/*---------------------OTRAS_INSTALACIONES---------------------*/

-- Inserciones a la tabla otras_instalaciones
INSERT INTO otras_instalaciones (id_autorizacion, id_establecimiento, tipo_actividad, descripcion_actividad, coste_actividad)
VALUES
    (007, 2, 'reclamo_publicitario', 'Publicidad de Zara en el acceso al local', 100.00),
    (008, 3, 'rampa_acceso', 'Rampa de acceso al establecimiento adaptada para personas con algún tipo de discapacidad física', 500.00),
    (009, 3, 'actividad_divulgativa', 'Instalación para actividades con el fin de fomentar ideas u opiniones sobre la salud mental', 0);


/*---------------------APROVECHAMIENTO_ESPECIAL---------------------*/

-- Inserciones a la tabla aprovechamiento_especial
INSERT INTO aprovechamiento_especial(id_autorizacion, aportacion_economica_minima, ubicacion_parcela)
VALUES
    (010, 10000.00, 'Parcela en Calle Larios 15'),
    (011, 5000.00, 'Avenida de Andalucía, 4');

/*---------------------ELEMENTO_DESMONTABLE---------------------*/

-- Inserciones a la tabla elemento_desmontable
INSERT INTO elemento_desmontable (tipo_desmontable, id_autorizacion, numero_elementos_desmontables, caracteristicas_elemento_desmontable)
VALUES
    ('mesa', 001, 10, 'Mesas de madera con acabado rústico'),
    ('silla', 001, 30, 'Sillas de madera con acabado rústico'),
    ('silla', 002, 40, 'Sillas de plástico de color rojo'),
    ('sombrilla', 003, 5, 'Sombrillas grandes de tela impermeable');



/*---------------------TITULARIDAD---------------------*/

-- Inserciones a la tabla titularidad
INSERT INTO titularidad(dni_o_cif_solicitante, id_establecimiento, fecha_inicio_titularidad, fecha_fin_titularidad)
VALUES

('71311220R', 1, '2023-01-01', NULL),
('A12345678', 2, '2023-12-01', '2025-06-30'),
('71456892F', 3, '2022-11-26', NULL),
('71311220R', 4, '2021-08-31', NULL),
('56745678A', 15, '2022-03-01', '2024-06-30');/*ha acabado titularidad*/


/*****************************************************/
/*---------------------Consultas---------------------*/
/*****************************************************/

/*SOLICITANTE*/
/*Obtener todos los solicitantes con convenio*/
SELECT *
FROM SOLICITANTE
WHERE hay_convenio='1';

/*Obtener el numero de solicitantes con convenio*/
SELECT COUNT(*) AS solicitantes_con_convenio
FROM SOLICITANTE 
WHERE hay_convenio = '1';


/*AUTORIZACION*/
/*Obtener el numero de infracciones cometidas por cada solicitante*/

SELECT A.dni_o_cif_solicitante , COUNT(*) AS total_infracciones
FROM autorizacion A natural join infraccion I
GROUP BY dni_o_cif_solicitante
HAVING COUNT(*) >= 1;



/*Obtener el total de solicitantes que tienen más de una autorización*/
 SELECT dni_o_cif_solicitante, COUNT(*) AS total_autorizaciones
 FROM AUTORIZACION
 GROUP BY dni_o_cif_solicitante
 HAVING COUNT(*) > 1;

/*Obtener las autorizaciones cuyo horario de fin autorizado sea mas pronto de las diez de la noche*/
 SELECT * 
FROM AUTORIZACION 
WHERE horario_fin_autorizado < '22:00:00';


/*Obtener todas las autorizaciones que han sido revocadas*/
SELECT *
FROM AUTORIZACION
WHERE fecha_revocacion IS NOT NULL;


/*Obtenr las autorizaciones ordenadas de mayor a menor fianza*/
SELECT * 
FROM AUTORIZACION 
ORDER BY fianza DESC;

/*ESTABLECIMIENTO*/    
/*Obtener todos los establecimientos con una superficie mayor a 10 m2*/
SELECT *
FROM ESTABLECIMIENTO
WHERE superficie > 10;

/*Establecimientos con instalacion electrica y que su solicitante tenga convenio*/
SELECT E.id_establecimiento
FROM establecimiento E natural join solicitante S
WHERE S.hay_convenio='1' AND E.instalacion_electrica='1';


/*Obtener los establecimientos de mayor a menor superficie*/
SELECT *
FROM ESTABLECIMIENTO 
ORDER BY superficie DESC;

/*INFRACCION*/
/*Obtener todas las infracciones graves*/
SELECT *
FROM INFRACCION
WHERE grado_infraccion = 'Grave';

/*Obtener todas las infracciones con una sancion mayor a 1000*/
SELECT * 
FROM INFRACCION 
WHERE cantidad_sancion > 1000;

/*Obtener el numero de infracciones que se han cometido por cada grado de infraccion*/
SELECT grado_infraccion, COUNT(*) AS total_infracciones
FROM INFRACCION
GROUP BY grado_infraccion;

/*Obtener los dnis de los solicitantes que recibieron infracciones de grado Muy Grave*/
SELECT A.dni_o_cif_solicitante
FROM autorizacion A natural join infraccion I
WHERE grado_infraccion='Muy Grave';

/*TERRAZA*/
/*Obtener todas las terrazas con espacios privados de uso público*/
 SELECT *
 FROM TERRAZA
 WHERE  espacios_privados_uso_publico IS NOT NULL
 AND espacios_privados_uso_publico = '1';

/*Obtener la modalidad de todas las terrazas que tiene el solicitante con dni A12345678*/
SELECT T.modalidad_ocupacion
FROM terraza T natural join autorizacion A
WHERE A.dni_o_cif_solicitante='A12345678';

/*TOLDOS Y VOLADIZOS*/
/*Obtener todos los toldos con más de un cerramiento*/
SELECT *
FROM TOLDOS_Y_VOLADIZOS
WHERE numero_cerramientos > 1;

/*Obtener la ubicacion del establecimiento de todas autorizaciones que pidan voladizos*/
SELECT E.ubicacion_establecimiento
FROM toldos_y_voladizos T natural join establecimiento E
WHERE T.tipo_fijo='voladizo';

/*OTRAS INSTALACIONES*/
/*Obtener todas las rampas de acceso con coste superior a 300*/
SELECT *
FROM OTRAS_INSTALACIONES
WHERE tipo_actividad = 'rampa_acceso' AND coste_actividad > 300;
 
 /*APROVECHAMIENTO ESPECIAL*/
 /*Obtener todos los aprovechamientos especiales con una aportación mínima superior a 5000€*/
 SELECT *
 FROM APROVECHAMIENTO_ESPECIAL
 WHERE aportacion_economica_minima > 5000;

 /*ELEMENTO DESMONTABLE*/
 /*Obtener todos los elementos desmontables con mas de 30 unidades*/
 SELECT *
 FROM ELEMENTO_DESMONTABLE
 WHERE numero_elementos_desmontables > 30;

/*Obtener las autorizaciones para pedir mesas*/
SELECT * 
FROM elemento_desmontable
WHERE tipo_desmontable = 'mesa';

/*Obtener los dnis de los solicitantes que hayan pedido una silla*/
SELECT A.dni_o_cif_solicitante
FROM ELEMENTO_DESMONTABLE E natural join terraza T natural join autorizacion A 
WHERE E.tipo_desmontable='silla';


 /*TITULARIDAD*/

 /*Obtener todos los titulares que estuvieron en el año 2023*/
 
SELECT *
FROM TITULARIDAD
WHERE 
    (fecha_inicio_titularidad <= '2023-12-31' AND (fecha_fin_titularidad IS NULL OR fecha_fin_titularidad >= '2023-01-01'));


 /*Obtener todas las titularidades activas*/

SELECT *
FROM TITULARIDAD
WHERE fecha_fin_titularidad IS NULL OR (fecha_fin_titularidad>=CURRENT_DATE);

 

 /*Obtener las ubicaciones de los establecimientos de cada titular*/
SELECT T.dni_o_cif_solicitante, E.ubicacion_establecimiento
FROM TITULARIDAD T
JOIN ESTABLECIMIENTO E ON T.id_establecimiento=E.id_establecimiento;
