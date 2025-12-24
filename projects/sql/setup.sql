-- su - postgres
-- sql template1

-- to generate the password:
-- pwgen -c 25 | head
create user ores with password 'ores';
create database ores;
grant all privileges on database ores to ores;
