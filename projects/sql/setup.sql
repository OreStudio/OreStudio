-- su - postgres
-- sql template1

create user ores with password 'ores';
create database oresdb;
grant all privileges on database oresdb to ores;
