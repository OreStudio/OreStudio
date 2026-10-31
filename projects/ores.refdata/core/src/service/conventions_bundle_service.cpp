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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "convention_family_apply.hpp"
#include "conventions_identity.hpp"
#include "conventions_row_helpers.hpp"
#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
#include "ores.refdata.core/service/conventions_bundle.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include "ores.refdata.api/domain/convention_codec.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short_codec.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive_codec.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::refdata::service {

using namespace ores::refdata::repository;

namespace {

// The one convention family that spans a central row and its two parts. Those
// rows are resolved and written together, because the identity is shared and a
// part names its central row.
constexpr std::string_view central_family = "convention";
constexpr std::string_view pay_receive_family = "tenor_basis_swap_pay_receive";
constexpr std::string_view long_short_family = "tenor_basis_swap_long_short";

/**
 * @brief Where the registry lists a family, or past the end when it does not.
 */
consteval std::size_t conventions_bundle_service_family_position(std::string_view key) {
    for (std::size_t i = 0; i < domain::convention_family_count; ++i)
        if (domain::convention_families[i].key == key)
            return i;
    return domain::convention_family_count;
}

// The read keeps a part only when it already carries that part's central row,
// so each of the three keys must be in the registry and the shared family must
// come first.
static_assert(conventions_bundle_service_family_position(central_family) <
                  domain::convention_family_count,
              "The registry has no convention family.");
static_assert(conventions_bundle_service_family_position(pay_receive_family) <
                  domain::convention_family_count,
              "The registry has no tenor basis swap pay receive family.");
static_assert(conventions_bundle_service_family_position(long_short_family) <
                  domain::convention_family_count,
              "The registry has no tenor basis swap long short family.");
static_assert(conventions_bundle_service_family_position(central_family) <
                  conventions_bundle_service_family_position(pay_receive_family),
              "The read needs the shared family before its pay receive part.");
static_assert(conventions_bundle_service_family_position(central_family) <
                  conventions_bundle_service_family_position(long_short_family),
              "The read needs the shared family before its long short part.");

bool belongs_to_the_tenor_bundle(const std::string& family) {
    return family == central_family || family == pay_receive_family || family == long_short_family;
}

/**
 * @brief Decodes every payload a family carries into its own type.
 */
template <typename Row>
std::vector<Row> decode_family(const conventions_bundle& bundle,
                               std::string_view family,
                               const ores::database::context& ctx) {
    std::vector<Row> rows;
    for (const auto& row : bundle.rows) {
        if (row.family != family)
            continue;
        const auto decoded = ores::utility::codec::json::try_read<Row>(row.payload);
        if (!decoded)
            throw std::runtime_error("Bundle payload for family \"" + std::string(family) +
                                     "\" does not decode.");
        auto value = std::move(*decoded);
        // A grouped row keeps its security fields in a member, so the stamp
        // has to walk into it; stamp_document does, and does nothing extra
        // to a flat row.
        ores::service::messaging::stamp_document(value, ctx);
        rows.push_back(std::move(value));
    }
    return rows;
}

}

conventions_bundle_service::conventions_bundle_service(context ctx)
    : ctx_(std::move(ctx)) {}

conventions_bundle conventions_bundle_service::read() {
    conventions_bundle bundle;
    // The two part families belong to a central row the bundle must also carry,
    // and the part tables would accept one that names nothing. The central
    // family is read first, because registry order puts it before its parts.
    std::set<std::string> covered_central;
    for (std::size_t i = 0; i < domain::convention_family_count; ++i) {
        const auto family = std::string(domain::convention_families[i].key);
        const auto payloads = read_family_payloads(ctx_, family);
        int sequence = 0;
        for (const auto& payload : payloads) {
            // The shared convention table carries every kind, and this bundle
            // covers one of them.
            if (family == central_family) {
                const auto decoded = ores::utility::codec::json::try_read<domain::convention>(payload);
                if (!decoded || decoded->identity.kind_code != tenor_basis_swap_kind)
                    continue;
                covered_central.insert(boost::uuids::to_string(decoded->identity.id));
            }
            if (family == pay_receive_family) {
                const auto decoded = ores::utility::codec::json::try_read<domain::tenor_basis_swap_pay_receive>(payload);
                if (!decoded || !covered_central.contains(
                                    boost::uuids::to_string(decoded->identity.convention_id)))
                    continue;
            }
            if (family == long_short_family) {
                const auto decoded = ores::utility::codec::json::try_read<domain::tenor_basis_swap_long_short>(payload);
                if (!decoded || !covered_central.contains(
                                    boost::uuids::to_string(decoded->identity.convention_id)))
                    continue;
            }
            bundle.rows.push_back(conventions_bundle_row{
                .family = family, .sequence = sequence++, .payload = payload});
        }
    }
    return bundle;
}

conventions_apply_result conventions_bundle_service::apply(const conventions_bundle& bundle) {
    conventions_apply_result result;

    auto central = decode_family<domain::convention>(bundle, central_family, ctx_);
    auto pay_receive =
        decode_family<domain::tenor_basis_swap_pay_receive>(bundle, pay_receive_family, ctx_);
    auto long_short =
        decode_family<domain::tenor_basis_swap_long_short>(bundle, long_short_family, ctx_);

    resolve_tenor_basis_swap_identity(ctx_, central, pay_receive, long_short);
    const auto forms = tenor_basis_swap_forms_of(central, pay_receive, long_short);

    // The central row is written before its parts, because a part names it.
    replace_rows_by_id(ctx_, convention_repository(), std::move(central));
    replace_rows_by_id(ctx_, tenor_basis_swap_pay_receive_repository(), std::move(pay_receive));
    replace_rows_by_id(ctx_, tenor_basis_swap_long_short_repository(), std::move(long_short));

    for (const auto& row : bundle.rows) {
        if (belongs_to_the_tenor_bundle(row.family))
            continue;
        if (!apply_staged_family(ctx_, row.family, row.payload, result))
            throw std::runtime_error("The registry does not know the convention family '" +
                                     row.family + "'.");
    }

    close_omitted_leg_forms(ctx_, forms);
    return result;
}

}
