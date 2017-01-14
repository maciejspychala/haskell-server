create table users
( id bigserial primary key not null,
    first_name varchar(40) not null,
    second_name varchar(40) not null,
    team integer
);

insert into users (first_name, second_name, team) values
('maciek', 'spychala', 1),
('magda', 'siemiawska', 2),
('marcin', 'chmiel', 3),
('antoni', 'walewski', 4),
('tomek', 'zoliborski', 2),
('wieslaw', 'aramewicz', 3),
('kasia', 'bolera', 2);

create table teams (
    id bigserial primary key not null,
    name varchar(40)
);

insert into teams (name) values
('frontend'),
('backend'),
('design'),
('marketing'),
('management');

CREATE TABLE "tasks" (
      "id" BIGSERIAL PRIMARY KEY,
      "priority" INTEGER NOT NULL,
      "importance" INTEGER NOT NULL,
      "begin_date" TIMESTAMP NOT NULL,
      "end_date" TIMESTAMP NOT NULL,
      "description" TEXT NOT NULL,
      "team" INTEGER NOT NULL
);

ALTER TABLE "tasks" ADD CONSTRAINT "fk_task__team" FOREIGN KEY ("team") REFERENCES "teams" ("id");


insert into tasks (priority, importance, begin_date, end_date, description, team) values
(1, 1, '2017-01-13', '2017-01-16', 'make login page', 1),
(2, 1, '2017-01-15', '2017-01-26', 'refactor code', 2),
(1, 3, '2017-01-16', '2017-01-22', 'prepare mockups', 3),
(2, 1, '2017-01-12', '2017-01-29', 'start sale', 4),
(1, 3, '2017-01-12', '2017-01-31', 'hire more people', 5),
(1, 1, '2017-02-03', '2017-02-16', 'better animations', 1),
(2, 1, '2017-02-05', '2017-02-16', 'jump to haskell', 2),
(1, 3, '2017-02-06', '2017-02-12', 'new logo', 3),
(2, 1, '2017-02-02', '2017-02-19', 'analyse analytics', 4),
(1, 3, '2017-02-02', '2017-02-21', 'do not respond to emails', 5);

CREATE TABLE "events" (
    "id" BIGSERIAL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "creator" INTEGER NOT NULL
);

CREATE INDEX "idx_event__creator" ON "events" ("creator");

ALTER TABLE "events" ADD CONSTRAINT "fk_event__creator" FOREIGN KEY ("creator") REFERENCES "users" ("id");

insert into events (name, creator) values
('George birthday', 5),
('Srump details', 2),
('New year', 3),
('Why you should start programming in Haskell - presentation', 1),
('Pizza party', 4);

CREATE TABLE "checklists" (
      "id" SERIAL PRIMARY KEY,
      "task" INTEGER,
      "user" INTEGER
);

CREATE INDEX "idx_checklist__user" ON "checklists" ("user");

CREATE INDEX "idx_checklist__task" ON "checklists" ("task");

ALTER TABLE "checklists" ADD CONSTRAINT "fk_checklist__user" FOREIGN KEY ("user") REFERENCES "users" ("id");

ALTER TABLE "checklists" ADD CONSTRAINT "fk_checklist__task" FOREIGN KEY ("task") REFERENCES "tasks" ("id");

CREATE TABLE "checklistitems" (
      "id" SERIAL PRIMARY KEY,
      "name" TEXT NOT NULL,
      "finished" BOOLEAN NOT NULL,
      "checklist" INTEGER NOT NULL
);

CREATE INDEX "idx_checklistitem__checklist" ON "checklistitems" ("checklist");

ALTER TABLE "checklistitems" ADD CONSTRAINT "fk_checklistitem__checklist" FOREIGN KEY ("checklist") REFERENCES "checklists" ("id");

insert into checklists (task) values
(1),
(2),
(3),
(4),
(5);

insert into checklistitems (name, finished, checklist) values
('reformat code', true, 1),
('delete debug message', true, 1),
('devide Order into subclasses', false, 1);
