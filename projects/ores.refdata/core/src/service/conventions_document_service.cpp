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
#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.core/repository/average_ois_convention_repository.hpp"
#include "ores.refdata.core/repository/bma_basis_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/bond_yield_convention_repository.hpp"
#include "ores.refdata.core/repository/cds_convention_repository.hpp"
#include "ores.refdata.core/repository/cms_spread_option_convention_repository.hpp"
#include "ores.refdata.core/repository/commodity_forward_convention_repository.hpp"
#include "ores.refdata.core/repository/commodity_future_convention_repository.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
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
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_two_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/zero_convention_repository.hpp"
#include "ores.refdata.core/repository/zero_inflation_index_convention_repository.hpp"
#include "ores.refdata.core/service/convention_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace ores::refdata::service {

using namespace ores::refdata::repository;

namespace {

// A party's convention is identified by the party, the kind and the name the
// source document gives it, so a second import of that convention replaces the
// row the party holds rather than colliding with it.
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

// A row's version comes from the live row with the same key, so an import
// replaces the row it names rather than colliding with it. The central row and
// its parts are addressed by the key the identity resolved to, which the
// resolver below sets, so the id alone is the key by the time a row is written.
template <typename Repository, typename Row>
void replace_rows_by_id(const ores::database::context& ctx,
                        Repository repo,
                        std::vector<Row> rows) {
    if (rows.empty())
        return;
    std::map<std::string, int> held;
    for (const auto& r : repo.read_latest(ctx))
        held[boost::uuids::to_string(r.id)] = r.version;
    for (auto& r : rows) {
        const auto it = held.find(boost::uuids::to_string(r.id));
        r.version = it == held.end() ? 0 : it->second;
    }
    repo.write(ctx, rows);
}

constexpr std::string_view tenor_basis_swap_kind = "TENOR_BASIS_SWAP";

boost::uuids::uuid new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

// A convention is identified by its party, its kind and the name it carries in
// the source document. The mapper states the kind and the name but cannot state
// the party, so it emits a provisional key per convention and the part rows
// name their central row by that key. Here the party is known, so the identity
// resolves through the generated finder: the row the party already holds keeps
// its key, and only a genuinely new convention is given one. The parts are
// relinked to the key the identity resolved to, and each takes the key of the
// part the party already holds for that convention, so a second import replaces
// the rows the first wrote rather than adding a second convention.
void resolve_tenor_basis_swap_identity(const ores::database::context& ctx,
                                       messaging::conventions_document& v) {
    convention_service conventions(ctx);
    std::map<std::string, boost::uuids::uuid> resolved;
    for (auto& c : v.convention) {
        if (c.kind_code != tenor_basis_swap_kind)
            continue;
        const auto provisional = c.id;
        const auto existing = conventions.find_convention_by_code(c.party_id, c.kind_code, c.name);
        c.id = existing ? existing->id : new_uuid();
        resolved[boost::uuids::to_string(provisional)] = c.id;
    }
    if (resolved.empty())
        return;

    const auto relink = [&](auto& parts, auto repo) {
        for (auto& p : parts) {
            const auto it = resolved.find(boost::uuids::to_string(p.convention_id));
            if (it == resolved.end())
                continue;
            p.convention_id = it->second;
            const auto held =
                repo.read_latest_by_convention_id(ctx,
                                                  boost::uuids::to_string(it->second),
                                                  0,
                                                  std::numeric_limits<std::uint32_t>::max(),
                                                  {},
                                                  std::nullopt);
            p.id = held.empty() ? new_uuid() : held.front().id;
        }
    };
    relink(v.tenor_basis_swap_pay_receive, tenor_basis_swap_pay_receive_repository());
    relink(v.tenor_basis_swap_long_short, tenor_basis_swap_long_short_repository());
}

// Closes every live row one part table holds for a convention.
template <typename Repository>
void close_live_rows(const ores::database::context& ctx,
                     Repository repo,
                     const std::string& convention_id) {
    const auto live = repo.read_latest_by_convention_id(
        ctx, convention_id, 0, std::numeric_limits<std::uint32_t>::max(), {}, std::nullopt);
    for (const auto& row : live)
        repo.remove(ctx, boost::uuids::to_string(row.id));
}

// A convention fills exactly one leg form. The part writes above make the form
// the document carries current; a live part the document omits is closed here,
// so an import that moves a convention from one form to the other does not
// leave both live. It runs after the writes, so a convention is never left with
// neither. This is the same obligation the DQ publish carries; the two cannot
// share a body, because this path writes through the repositories and the
// publish writes in SQL.
void close_omitted_leg_forms(const ores::database::context& ctx,
                             const std::vector<boost::uuids::uuid>& conventions,
                             const std::set<std::string>& pay_receive_forms,
                             const std::set<std::string>& long_short_forms) {
    for (const auto& id : conventions) {
        const auto key = boost::uuids::to_string(id);
        if (!pay_receive_forms.contains(key))
            close_live_rows(ctx, tenor_basis_swap_pay_receive_repository(), key);
        if (!long_short_forms.contains(key))
            close_live_rows(ctx, tenor_basis_swap_long_short_repository(), key);
    }
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
    resolve_tenor_basis_swap_identity(ctx_, v);

    // The leg forms the document carries, read before the parts move into the
    // write calls below, and the conventions they belong to.
    std::set<std::string> pay_receive_forms;
    for (const auto& p : v.tenor_basis_swap_pay_receive)
        pay_receive_forms.insert(boost::uuids::to_string(p.convention_id));
    std::set<std::string> long_short_forms;
    for (const auto& l : v.tenor_basis_swap_long_short)
        long_short_forms.insert(boost::uuids::to_string(l.convention_id));
    std::vector<boost::uuids::uuid> tenor_conventions;
    for (const auto& c : v.convention)
        if (c.kind_code == tenor_basis_swap_kind)
            tenor_conventions.push_back(c.id);

    conventions_save_result r;
    replace_party_rows(ctx_, zero_convention_repository(), std::move(v.zero));
    replace_party_rows(ctx_, average_ois_convention_repository(), std::move(v.average_ois));
    replace_party_rows(ctx_, bma_basis_swap_convention_repository(), std::move(v.bma_basis_swap));
    replace_party_rows(
        ctx_, cross_currency_basis_convention_repository(), std::move(v.cross_currency_basis));
    replace_party_rows(ctx_,
                       cross_currency_fix_float_convention_repository(),
                       std::move(v.cross_currency_fix_float));
    replace_rows_by_id(ctx_, convention_repository(), std::move(v.convention));
    replace_rows_by_id(
        ctx_, tenor_basis_swap_pay_receive_repository(), std::move(v.tenor_basis_swap_pay_receive));
    replace_rows_by_id(
        ctx_, tenor_basis_swap_long_short_repository(), std::move(v.tenor_basis_swap_long_short));
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
    close_omitted_leg_forms(ctx_, tenor_conventions, pay_receive_forms, long_short_forms);
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
    // The central row is shared by every kind, so only the rows whose kind is a
    // family this document carries are read, and each part joins by its key.
    const auto conventions = convention_repository().read_latest(ctx_);
    const auto pay_receives = tenor_basis_swap_pay_receive_repository().read_latest(ctx_);
    const auto long_shorts = tenor_basis_swap_long_short_repository().read_latest(ctx_);
    for (const auto& c : conventions) {
        if (c.kind_code != "TENOR_BASIS_SWAP")
            continue;
        r.convention.push_back(c);
        for (const auto& p : pay_receives)
            if (p.convention_id == c.id)
                r.tenor_basis_swap_pay_receive.push_back(p);
        for (const auto& l : long_shorts)
            if (l.convention_id == c.id)
                r.tenor_basis_swap_long_short.push_back(l);
    }
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
