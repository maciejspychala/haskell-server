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
