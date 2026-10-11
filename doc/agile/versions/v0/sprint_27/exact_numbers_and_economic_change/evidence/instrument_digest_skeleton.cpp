// Generated skeleton for the digest's instrument read. One arm per value of
// instrument_table (trade_type_routing.hpp). Drop this into
// trade_component_queries.cpp and fill the legs/amounts for the families
// that carry them; a family with no children needs only the first line.
//
// The label comes from the entity model: `:tablename:` minus the
// `ores_trading_` prefix and the `_tbl` suffix, which is the repository's
// own name.

switch (*table) {
    case instrument_table::balance_guaranteed_swap_instrument: {
        for (const auto& row : balance_guaranteed_swap_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::bond_instrument: {
        for (const auto& row : bond_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::callable_swap_instrument: {
        for (const auto& row : callable_swap_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::cap_floor_instrument: {
        for (const auto& row : cap_floor_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::commodity_instrument: {
        for (const auto& row : commodity_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::composite_instrument: {
        for (const auto& row : composite_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::credit_instrument: {
        for (const auto& row : credit_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::equity_accumulator_instrument: {
        for (const auto& row : equity_accumulator_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::equity_asian_option_instrument: {
        for (const auto& row : equity_asian_option_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::equity_barrier_option_instrument: {
        for (const auto& row : equity_barrier_option_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::equity_digital_option_instrument: {
        for (const auto& row : equity_digital_option_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::equity_forward_instrument: {
        for (const auto& row : equity_forward_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::equity_option_instrument: {
        for (const auto& row : equity_option_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::equity_position_instrument: {
        for (const auto& row : equity_position_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::equity_swap_instrument: {
        for (const auto& row : equity_swap_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::equity_variance_swap_instrument: {
        for (const auto& row : equity_variance_swap_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::fra_instrument: {
        for (const auto& row : fra_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::fx_accumulator_instrument: {
        for (const auto& row : fx_accumulator_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::fx_asian_forward_instrument: {
        for (const auto& row : fx_asian_forward_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::fx_barrier_option_instrument: {
        for (const auto& row : fx_barrier_option_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::fx_digital_option_instrument: {
        for (const auto& row : fx_digital_option_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::fx_forward_instrument: {
        for (const auto& row : fx_forward_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::fx_vanilla_option_instrument: {
        for (const auto& row : fx_vanilla_option_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::fx_variance_swap_instrument: {
        for (const auto& row : fx_variance_swap_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::inflation_swap_instrument: {
        for (const auto& row : inflation_swap_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::knock_out_swap_instrument: {
        for (const auto& row : knock_out_swap_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::scripted_instrument: {
        for (const auto& row : scripted_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::swaption_instrument: {
        for (const auto& row : swaption_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
    case instrument_table::vanilla_swap_instrument: {
        for (const auto& row : vanilla_swap_instrument_repository{}.read_latest(ctx, trade_id))
            component_digests.push_back(domain::economic_digest(row));
        break;
    }
}
