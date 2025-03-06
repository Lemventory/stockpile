CREATE TABLE menu_items (
  sort INT NOT NULL,
  sku UUID PRIMARY KEY,
  brand TEXT NOT NULL,
  name TEXT NOT NULL,
  price DECIMAL(10,2) NOT NULL,
  measure_unit TEXT NOT NULL,
  per_package TEXT NOT NULL,
  quantity INT NOT NULL,
  category TEXT NOT NULL,
  subcategory TEXT NOT NULL,
  description TEXT NOT NULL,
  tags TEXT[] NOT NULL,
  effects TEXT[] NOT NULL
);

CREATE TABLE strain_lineage (
  sku UUID PRIMARY KEY REFERENCES menu_items(sku) ON DELETE CASCADE,
  thc TEXT NOT NULL,
  cbg TEXT NOT NULL,
  strain TEXT NOT NULL,
  creator TEXT NOT NULL,
  species TEXT NOT NULL,
  dominant_terpene TEXT NOT NULL,
  terpenes TEXT[] NOT NULL,
  lineage TEXT[] NOT NULL,
  leafly_url TEXT NOT NULL,
  img TEXT NOT NULL
);