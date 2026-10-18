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
#ifndef ORES_REFDATA_CORE_SERVICE_CONVENTIONS_IDENTITY_HPP
#define ORES_REFDATA_CORE_SERVICE_CONVENTIONS_IDENTITY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include <boost/uuid/uuid.hpp>
#include <set>
#include <string>
#include <string_view>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief The kind a tenor basis swap central row carries.
 */
inline constexpr std::string_view tenor_basis_swap_kind = "TENOR_BASIS_SWAP";

/**
 * @brief What a bundle carries of the one convention family that spans a central row and two parts.
 *
 * The leg forms are read before the rows move, because a write takes them, and
 * the closure step needs to know which form the caller carried.
 */
struct tenor_basis_swap_forms {
    std::vector<boost::uuids::uuid> central;
    std::set<std::string> pay_receive;
    std::set<std::string> long_short;
};

/**
 * @brief The identities and leg forms a set of tenor basis swap rows carries.
 */
tenor_basis_swap_forms tenor_basis_swap_forms_of(
    const std::vector<domain::convention>& central,
    const std::vector<domain::tenor_basis_swap_pay_receive>& pay_receive,
    const std::vector<domain::tenor_basis_swap_long_short>& long_short);

/**
 * @brief Resolves the identity of every tenor basis swap the rows carry.
 *
 * A convention is identified by its party, its kind and the name it carries in
 * the source document. The mapper states the kind and the name but cannot state
 * the party, so it emits a provisional key per convention and the part rows
 * name their central row by that key. Here the party is known, so the identity
 * resolves through the generated finder: the row the party already holds keeps
 * its key, and only a genuinely new convention is given one. The parts are
 * relinked to the key the identity resolved to, and each takes the key of the
 * part the party already holds for that convention, so a second import replaces
 * the rows the first wrote rather than adding a second convention.
 *
 * The rows must be stamped, because the identity includes the party.
 */
void resolve_tenor_basis_swap_identity(
    const ores::database::context& ctx,
    std::vector<domain::convention>& central,
    std::vector<domain::tenor_basis_swap_pay_receive>& pay_receive,
    std::vector<domain::tenor_basis_swap_long_short>& long_short);

/**
 * @brief Closes every live leg form a convention carries and the rows omit.
 *
 * A convention fills exactly one leg form. The writes make the form the rows
 * carry current, and a live part the rows omit is closed here, so an import
 * that moves a convention from one form to the other does not leave both live.
 * It runs after the writes, so a convention is never left with neither.
 */
void close_omitted_leg_forms(const ores::database::context& ctx,
                             const tenor_basis_swap_forms& forms);

}

#endif
