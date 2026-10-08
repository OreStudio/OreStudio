-- Party visibility helpers for a row that belongs to a trade or a structure.
drop function if exists ores_trading_trade_party_fn(uuid, uuid);
drop function if exists ores_trading_structure_party_fn(uuid, uuid);
