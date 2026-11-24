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
 */
#ifndef ORES_MARKETDATA_SERVICE_APP_DERIVED_MARKET_OBJECT_KIND_HPP
#define ORES_MARKETDATA_SERVICE_APP_DERIVED_MARKET_OBJECT_KIND_HPP

#include "ores.marketdata.service/app/curve_publish_gate.hpp"
#include "ores.marketdata.service/app/curve_withdrawal.hpp"
#include "ores.marketdata.service/app/fx_forward_publish_service.hpp"
#include "ores.marketdata.service/app/fx_vol_surface_publish_service.hpp"
#include "ores.marketdata.service/export.hpp"
#include <array>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::service::app {

/**
 * @brief Which kind of derived market object a lifecycle serves.
 *
 * A kind says what one set is a set of. The acts of the lifecycle read and
 * write the same facts for every kind, and a kind names the four parts that
 * differ: its subject, its compute, its checks and its approval kinds.
 */
enum class derived_kind { curve, fx_forward, fx_vol_surface };

/**
 * @brief What a kind declares, in one row.
 *
 * The publish and withdrawal approval kinds are named by their codes and not by
 * their permissions, because the permissions, the approval count and the
 * auto-approval rule are read from the kind's own row at the raise: a tenant
 * changes who decides without a release.
 *
 * A kind that declares no withdrawal kind cannot take a published set out of
 * use. That is a fact of the declaration and not of the code that applies the
 * withdrawal, so the raise of a withdrawal is refused for such a kind before a
 * request is written.
 */
struct kind_declaration {
    derived_kind kind{derived_kind::curve};
    /// The approval kind a publish raises.
    std::string_view publish_kind_code;
    /// The approval kind a withdrawal raises, empty when the kind has none.
    std::string_view withdrawal_kind_code;
    /// The table the kind's subject part lives in.
    std::string_view subject_table;
};

/**
 * @brief Every kind the lifecycle serves, in one place.
 *
 * A fourth kind is a row here and a declaration of the four parts beside it,
 * and no step of the lifecycle changes.
 */
inline constexpr std::array<kind_declaration, 3> kind_declarations{{
    {derived_kind::curve,
     curve_publish_kind_code,
     curve_withdrawal_kind_code,
     "ores_marketdata_curve_publishes_tbl"},
    {derived_kind::fx_forward,
     fx_forward_publish_kind_code,
     {},
     "ores_marketdata_fx_forward_publishes_tbl"},
    {derived_kind::fx_vol_surface,
     fx_vol_surface_publish_kind_code,
     {},
     "ores_marketdata_fx_vol_surface_publishes_tbl"},
}};

/**
 * @brief The declaration of the kind a publish kind code names, if there is one.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::optional<kind_declaration>
declaration_of_publish_kind(std::string_view publish_kind_code);

/**
 * @brief The declaration of a kind.
 */
ORES_MARKETDATA_SERVICE_EXPORT const kind_declaration& declaration_of(derived_kind kind);

/**
 * @brief Whether the kind can take a published set out of use.
 */
ORES_MARKETDATA_SERVICE_EXPORT bool can_withdraw(const kind_declaration& declaration);

/**
 * @brief Why a withdrawal of this kind is refused at the raise, or nothing.
 *
 * The refusal is the declaration's, and the raise is where it is answered, so a
 * request that could never be applied is never written and no decider is asked
 * to decide it.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::optional<std::string>
withdrawal_refusal_of_publish_kind(std::string_view publish_kind_code);

}

#endif
