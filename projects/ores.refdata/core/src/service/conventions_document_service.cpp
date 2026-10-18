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
#include "convention_family_apply.hpp"
#include "conventions_identity.hpp"
#include "conventions_row_helpers.hpp"
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
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <utility>
#include <vector>

namespace ores::refdata::service {


using namespace ores::refdata::repository;

// The generated per-family apply lives in convention_family_apply.cpp.

conventions_document_service::conventions_document_service(context ctx)
    : ctx_(std::move(ctx)) {}

conventions_save_result conventions_document_service::save(messaging::conventions_document v) {
    ores::service::messaging::stamp_document(v, ctx_);
    resolve_tenor_basis_swap_identity(
        ctx_, v.convention, v.tenor_basis_swap_pay_receive, v.tenor_basis_swap_long_short);

    // The leg forms the document carries, read before the parts move into the
    // write calls below, and the conventions they belong to.
    const auto forms = tenor_basis_swap_forms_of(
        v.convention, v.tenor_basis_swap_pay_receive, v.tenor_basis_swap_long_short);

    conventions_save_result r;
    apply_party_families(ctx_, v);
    apply_families_by_id(ctx_, v);
    apply_world_families(ctx_, v, r);
    close_omitted_leg_forms(ctx_, forms);
    for (const auto& fx : v.fx)
        r.fx_skipped.push_back(fx.pair.base_currency + "-" + fx.pair.quote_currency +
                               "-FX-CONVENTIONS");
    return r;
}

messaging::conventions_document conventions_document_service::get() {
    messaging::conventions_document r;
    read_party_families(ctx_, r);
    read_families_by_id(ctx_, r);
    read_world_families(ctx_, r);

    // The central row is shared by every kind, so only the rows whose kind is a
    // family this document carries are kept, and each part joins by its key.
    const auto conventions = std::move(r.convention);
    const auto pay_receives = std::move(r.tenor_basis_swap_pay_receive);
    const auto long_shorts = std::move(r.tenor_basis_swap_long_short);
    r.convention.clear();
    r.tenor_basis_swap_pay_receive.clear();
    r.tenor_basis_swap_long_short.clear();
    for (const auto& c : conventions) {
        if (c.kind_code != tenor_basis_swap_kind)
            continue;
        r.convention.push_back(c);
        for (const auto& p : pay_receives)
            if (p.convention_id == c.id)
                r.tenor_basis_swap_pay_receive.push_back(p);
        for (const auto& l : long_shorts)
            if (l.convention_id == c.id)
                r.tenor_basis_swap_long_short.push_back(l);
    }
    return r;
}

}
