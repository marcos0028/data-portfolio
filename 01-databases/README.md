# Málaga Municipal Ordinance — Database Design and SQL

## Overview

This project focuses on the design and implementation of a relational database based on a municipal ordinance regulating the use of public spaces in Málaga, Spain.

The goal was to translate the requirements of a real-world regulatory framework into a structured relational data model, ensuring data consistency and representing the relationships between applicants, authorizations, establishments, and potential infringements.

## Project Objectives

- Analyse the requirements of a municipal ordinance.
- Design an Entity-Relationship (ER) diagram representing the main entities and their relationships.
- Define relational tables, primary keys, foreign keys, and attributes.
- Translate the conceptual model into a relational database structure.
- Write SQL queries to retrieve and analyse information stored in the database.

## Database Design

The ER diagram models the main components of the regulatory framework, including:

- **Applicants (`Solicitante`):** Identification and applicant classification.
- **Authorizations (`Autorizacion`):** Permitted activities, fees, authorised schedules, validity periods, and revocations.
- **Establishments (`Establecimiento`):** Establishment information, opening hours, surface area, and special-use permissions.
- **Infringements (`Infraccion`):** Recorded violations, dates, severity, and associated penalties.
- **Terraces (`Terraza`):** Occupation arrangements and use of private or public space.
- **Awnings and Canopies (`Toldos_y_voladizos`):** Characteristics of authorised structures.
- **Other Installations (`Otras_instalaciones`):** Additional activities and installations.
- **Special Use (`Aprovechamiento_especial`):** Economic contributions and plot location.
- **Removable Elements (`Elemento_desmontable`):** Types and characteristics of removable structures.

The model also represents relationships between these entities, including the inheritance of authorizations into different installation types.

## SQL

After designing the ER diagram, the conceptual model was translated into relational tables, defining keys and relationships.

SQL queries were then developed to retrieve information from the database and work with the relationships between its entities.

## Skills Demonstrated

- Relational database design
- Entity-Relationship modelling
- Relational schema design
- Primary and foreign key definition
- SQL querying
- Translating real-world requirements into structured data models

## Project Files

- `README.md`: Project overview and documentation.
- `er_diagram.png`: Entity-Relationship diagram.
- `sql/`: SQL scripts for table definitions and queries.