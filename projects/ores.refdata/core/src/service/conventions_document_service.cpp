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
#include "ores.refdata.core/service/conventions_document_service.hpp"
#include "ores.database/repository/document_operations.hpp"
#include "ores.refdata.core/repository/average_ois_convention_repository.hpp"
#include "ores.refdata.core/repository/bma_basis_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/bond_yield_convention_repository.hpp"
#include "ores.refdata.core/repository/cds_convention_repository.hpp"
#include "ores.refdata.core/repository/cms_spread_option_convention_repository.hpp"
#include "ores.refdata.core/repository/commodity_forward_convention_repository.hpp"
#include "ores.refdata.core/repository/commodity_future_convention_repository.hpp"
#include "ores.refdata.core/repository/cross_currency_basis_convention_repository.hpp"
#include "ores.refdata.core/repository/cross_currency_fix_float_convention_repository.hpp"
#include "ores.refdata.core/repository/deposit_convention_repository.hpp"
#include "ores.refdata.core/repository/fra_convention_repository.hpp"
#include "ores.refdata.core/repository/future_convention_repository.hpp"
#include "ores.refdata.core/repository/fx_option_convention_repository.hpp"
#include "ores.refdata.core/repository/ibor_index_convention_repository.hpp"
#include "ores.refdata.core/repository/inflation_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/intraday_power_load_convention_repository.hpp"
#include "ores.refdata.core/repository/ois_convention_repository.hpp"
#include "ores.refdata.core/repository/overnight_index_convention_repository.hpp"
#include "ores.refdata.core/repository/swap_convention_repository.hpp"
#include "ores.refdata.core/repository/swap_index_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_two_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/zero_convention_repository.hpp"
#include "ores.refdata.core/repository/zero_inflation_index_convention_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid.hpp>
#include <map>
#include <set>
#include <utility>

namespace ores::refdata::service {

using namespace ores::refdata::repository;

namespace {

// A party's convention is keyed by its ORE id and the party, so a second import
// of the same id replaces the row the party holds rather than colliding with it.
template <typename Repository, typename Row>
void replace_party_rows(const ores::database::context& ctx,
                        Repository repo,
                        std::vector<Row> rows) {
    if (rows.empty())
        return;
    std::map<std::pair<std::string, boost::uuids::uuid>, int> held;
    for (const auto& r : repo.read_latest(ctx))
        held[{r.id, r.party_id}] = r.version;
    for (auto& r : rows) {
        const auto it = held.find({r.id, r.party_id});
        r.version = it == held.end() ? 0 : it->second;
    }
    repo.write(ctx, rows);
}

// A world convention is the tenant's, whoever imports a document naming it, so
// one the tenant holds is never replaced by an import.
template <typename Repository, typename Row>
void add_missing_world_rows(const ores::database::context& ctx,
                            Repository repo,
                            const std::vector<Row>& rows,
                            std::vector<std::string>& kept) {
    std::set<std::string> held;
    for (const auto& r : repo.read_latest(ctx))
        held.insert(r.id);
    std::vector<Row> missing;
    for (const auto& r : rows) {
        if (held.contains(r.id))
            kept.push_back(r.id);
        else
            missing.push_back(r);
    }
    if (!missing.empty())
        repo.write(ctx, missing);
}

}

conventions_document_service::conventions_document_service(context ctx)
    : ctx_(std::move(ctx)) {}

conventions_save_result conventions_document_service::save(messaging::conventions_document v) {
    ores::service::messaging::stamp_document(v, ctx_);
    conventions_save_result r;
    replace_party_rows(ctx_, zero_convention_repository(), std::move(v.zero));
    replace_party_rows(ctx_, average_ois_convention_repository(), std::move(v.average_ois));
    replace_party_rows(ctx_, bma_basis_swap_convention_repository(), std::move(v.bma_basis_swap));
    replace_party_rows(
        ctx_, cross_currency_basis_convention_repository(), std::move(v.cross_currency_basis));
    replace_party_rows(ctx_,
                       cross_currency_fix_float_convention_repository(),
                       std::move(v.cross_currency_fix_float));
    replace_party_rows(
        ctx_, tenor_basis_swap_convention_repository(), std::move(v.tenor_basis_swap));
    replace_party_rows(
        ctx_, tenor_basis_two_swap_convention_repository(), std::move(v.tenor_basis_two_swap));
    replace_party_rows(ctx_, deposit_convention_repository(), std::move(v.deposit));
    replace_party_rows(ctx_, swap_convention_repository(), std::move(v.swap));
    replace_party_rows(ctx_, swap_index_convention_repository(), std::move(v.swap_index));
    replace_party_rows(ctx_, future_convention_repository(), std::move(v.future));
    replace_party_rows(ctx_, fx_option_convention_repository(), std::move(v.fx_option));
    replace_party_rows(ctx_, inflation_swap_convention_repository(), std::move(v.inflation_swap));
    replace_party_rows(
        ctx_, intraday_power_load_convention_repository(), std::move(v.intraday_power_load));
    replace_party_rows(ctx_, ois_convention_repository(), std::move(v.ois));
    replace_party_rows(ctx_, fra_convention_repository(), std::move(v.fra));
    replace_party_rows(
        ctx_, zero_inflation_index_convention_repository(), std::move(v.zero_inflation_index));
    replace_party_rows(ctx_, cds_convention_repository(), std::move(v.cds));
    replace_party_rows(
        ctx_, cms_spread_option_convention_repository(), std::move(v.cms_spread_option));
    replace_party_rows(
        ctx_, commodity_future_convention_repository(), std::move(v.commodity_future));
    replace_party_rows(
        ctx_, commodity_forward_convention_repository(), std::move(v.commodity_forward));
    replace_party_rows(ctx_, bond_yield_convention_repository(), std::move(v.bond_yield));
    add_missing_world_rows(ctx_, ibor_index_convention_repository(), v.ibor_index, r.world_kept);
    add_missing_world_rows(
        ctx_, overnight_index_convention_repository(), v.overnight_index, r.world_kept);
    for (const auto& fx : v.fx)
        r.fx_skipped.push_back(fx.pair.base_currency + "-" + fx.pair.quote_currency +
                               "-FX-CONVENTIONS");
    return r;
}

messaging::conventions_document conventions_document_service::get() {
    messaging::conventions_document r;
    r.zero = zero_convention_repository().read_latest(ctx_);
    r.average_ois = average_ois_convention_repository().read_latest(ctx_);
    r.bma_basis_swap = bma_basis_swap_convention_repository().read_latest(ctx_);
    r.cross_currency_basis = cross_currency_basis_convention_repository().read_latest(ctx_);
    r.cross_currency_fix_float = cross_currency_fix_float_convention_repository().read_latest(ctx_);
    r.tenor_basis_swap = tenor_basis_swap_convention_repository().read_latest(ctx_);
    r.tenor_basis_two_swap = tenor_basis_two_swap_convention_repository().read_latest(ctx_);
    r.deposit = deposit_convention_repository().read_latest(ctx_);
    r.swap = swap_convention_repository().read_latest(ctx_);
    r.swap_index = swap_index_convention_repository().read_latest(ctx_);
    r.future = future_convention_repository().read_latest(ctx_);
    r.fx_option = fx_option_convention_repository().read_latest(ctx_);
    r.inflation_swap = inflation_swap_convention_repository().read_latest(ctx_);
    r.intraday_power_load = intraday_power_load_convention_repository().read_latest(ctx_);
    r.ois = ois_convention_repository().read_latest(ctx_);
    r.fra = fra_convention_repository().read_latest(ctx_);
    r.zero_inflation_index = zero_inflation_index_convention_repository().read_latest(ctx_);
    r.cds = cds_convention_repository().read_latest(ctx_);
    r.cms_spread_option = cms_spread_option_convention_repository().read_latest(ctx_);
    r.commodity_future = commodity_future_convention_repository().read_latest(ctx_);
    r.commodity_forward = commodity_forward_convention_repository().read_latest(ctx_);
    r.bond_yield = bond_yield_convention_repository().read_latest(ctx_);
    r.ibor_index = ibor_index_convention_repository().read_latest(ctx_);
    r.overnight_index = overnight_index_convention_repository().read_latest(ctx_);
    return r;
}

}
