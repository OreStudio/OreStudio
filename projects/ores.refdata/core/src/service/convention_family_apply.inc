/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_convention_family_apply.inc.mustache
 * To modify, update the template and regenerate.
 */


// Staged families: each branch decodes one family's payload into its own
// type, so no instantiation reflects more than one family.
ORES_REFDATA_CORE_EXPORT bool apply_staged_family(const ores::database::context& ctx,
                                                  const std::string& family,
                                                  const std::string& payload,
                                                  conventions_apply_result& result) {
    if (family == "average_ois") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::average_ois_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"average_ois\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::average_ois_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           average_ois_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "bma_basis_swap") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::bma_basis_swap_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"bma_basis_swap\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::bma_basis_swap_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           bma_basis_swap_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "bond_yield") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::bond_yield_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"bond_yield\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::bond_yield_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           bond_yield_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "cds") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::cds_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"cds\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::cds_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           cds_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "cms_spread_option") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::cms_spread_option_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"cms_spread_option\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::cms_spread_option_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           cms_spread_option_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "commodity_forward") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::commodity_forward_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"commodity_forward\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::commodity_forward_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           commodity_forward_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "commodity_future") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::commodity_future_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"commodity_future\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::commodity_future_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           commodity_future_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "convention") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"convention\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_rows_by_id(ctx,
                           convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "cross_currency_basis") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::cross_currency_basis_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"cross_currency_basis\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::cross_currency_basis_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           cross_currency_basis_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "cross_currency_fix_float") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::cross_currency_fix_float_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"cross_currency_fix_float\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::cross_currency_fix_float_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           cross_currency_fix_float_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "deposit") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::deposit_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"deposit\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::deposit_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           deposit_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "fra") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::fra_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"fra\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::fra_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           fra_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "future") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::future_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"future\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::future_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           future_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "fx_option") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::fx_option_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"fx_option\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::fx_option_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           fx_option_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "ibor_index") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::ibor_index_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"ibor_index\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::ibor_index_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        add_missing_world_rows(ctx,
                               ibor_index_convention_repository(),
                               std::move(rows),
                               result.world_kept);
        return true;
    }
    if (family == "inflation_swap") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::inflation_swap_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"inflation_swap\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::inflation_swap_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           inflation_swap_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "intraday_power_load") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::intraday_power_load_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"intraday_power_load\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::intraday_power_load_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           intraday_power_load_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "ois") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::ois_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"ois\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::ois_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           ois_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "overnight_index") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::overnight_index_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"overnight_index\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::overnight_index_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        add_missing_world_rows(ctx,
                               overnight_index_convention_repository(),
                               std::move(rows),
                               result.world_kept);
        return true;
    }
    if (family == "swap") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::swap_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"swap\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::swap_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           swap_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "swap_index") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::swap_index_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"swap_index\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::swap_index_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           swap_index_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "tenor_basis_swap_long_short") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::tenor_basis_swap_long_short>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"tenor_basis_swap_long_short\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::tenor_basis_swap_long_short>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_rows_by_id(ctx,
                           tenor_basis_swap_long_short_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "tenor_basis_swap_pay_receive") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::tenor_basis_swap_pay_receive>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"tenor_basis_swap_pay_receive\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::tenor_basis_swap_pay_receive>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_rows_by_id(ctx,
                           tenor_basis_swap_pay_receive_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "tenor_basis_two_swap") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::tenor_basis_two_swap_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"tenor_basis_two_swap\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::tenor_basis_two_swap_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           tenor_basis_two_swap_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "zero") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::zero_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"zero\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::zero_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           zero_convention_repository(),
                           std::move(rows));
        return true;
    }
    if (family == "zero_inflation_index") {
        const auto decoded =
            rfl::json::read<ores::refdata::domain::zero_inflation_index_convention>(payload);
        if (!decoded)
            throw std::runtime_error(
                "Staged conventions payload for family "
                "\"zero_inflation_index\" does not decode.");
        auto rows = std::vector<ores::refdata::domain::zero_inflation_index_convention>{
            std::move(*decoded)};
        ores::service::messaging::stamp(rows.front(), ctx);
        replace_party_rows(ctx,
                           zero_inflation_index_convention_repository(),
                           std::move(rows));
        return true;
    }
    return false;
}

// Payload reads: each branch reads one family's live rows and serialises each
// as its own type, so an in-process caller sees the bytes the wire carries and
// no branch touches another family's type.
ORES_REFDATA_CORE_EXPORT std::vector<std::string>
read_family_payloads(const ores::database::context& ctx, const std::string& family) {
    if (family == "average_ois") {
        std::vector<std::string> payloads;
        for (const auto& row : average_ois_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "bma_basis_swap") {
        std::vector<std::string> payloads;
        for (const auto& row : bma_basis_swap_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "bond_yield") {
        std::vector<std::string> payloads;
        for (const auto& row : bond_yield_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "cds") {
        std::vector<std::string> payloads;
        for (const auto& row : cds_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "cms_spread_option") {
        std::vector<std::string> payloads;
        for (const auto& row : cms_spread_option_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "commodity_forward") {
        std::vector<std::string> payloads;
        for (const auto& row : commodity_forward_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "commodity_future") {
        std::vector<std::string> payloads;
        for (const auto& row : commodity_future_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "convention") {
        std::vector<std::string> payloads;
        for (const auto& row : convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "cross_currency_basis") {
        std::vector<std::string> payloads;
        for (const auto& row : cross_currency_basis_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "cross_currency_fix_float") {
        std::vector<std::string> payloads;
        for (const auto& row : cross_currency_fix_float_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "deposit") {
        std::vector<std::string> payloads;
        for (const auto& row : deposit_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "fra") {
        std::vector<std::string> payloads;
        for (const auto& row : fra_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "future") {
        std::vector<std::string> payloads;
        for (const auto& row : future_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "fx_option") {
        std::vector<std::string> payloads;
        for (const auto& row : fx_option_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "ibor_index") {
        std::vector<std::string> payloads;
        for (const auto& row : ibor_index_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "inflation_swap") {
        std::vector<std::string> payloads;
        for (const auto& row : inflation_swap_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "intraday_power_load") {
        std::vector<std::string> payloads;
        for (const auto& row : intraday_power_load_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "ois") {
        std::vector<std::string> payloads;
        for (const auto& row : ois_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "overnight_index") {
        std::vector<std::string> payloads;
        for (const auto& row : overnight_index_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "swap") {
        std::vector<std::string> payloads;
        for (const auto& row : swap_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "swap_index") {
        std::vector<std::string> payloads;
        for (const auto& row : swap_index_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "tenor_basis_swap_long_short") {
        std::vector<std::string> payloads;
        for (const auto& row : tenor_basis_swap_long_short_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "tenor_basis_swap_pay_receive") {
        std::vector<std::string> payloads;
        for (const auto& row : tenor_basis_swap_pay_receive_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "tenor_basis_two_swap") {
        std::vector<std::string> payloads;
        for (const auto& row : tenor_basis_two_swap_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "zero") {
        std::vector<std::string> payloads;
        for (const auto& row : zero_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    if (family == "zero_inflation_index") {
        std::vector<std::string> payloads;
        for (const auto& row : zero_inflation_index_convention_repository().read_latest(ctx))
            payloads.push_back(rfl::json::write(row));
        return payloads;
    }
    throw std::logic_error("The registry does not know the convention family '" + family + "'.");
}
