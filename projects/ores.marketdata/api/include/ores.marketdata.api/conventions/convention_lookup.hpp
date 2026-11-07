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
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_API_CONVENTIONS_CONVENTION_LOOKUP_HPP
#define ORES_MARKETDATA_API_CONVENTIONS_CONVENTION_LOOKUP_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.marketdata.api/export.hpp"
#include "ores.refdata.api/domain/ibor_index_convention.hpp"
#include "ores.refdata.api/domain/ois_convention.hpp"
#include "ores.refdata.api/domain/overnight_index_convention.hpp"
#include "ores.refdata.api/domain/swap_convention.hpp"
#include <expected>
#include <string>
#include <variant>
#include <vector>

namespace ores::marketdata::conventions {

/**
 * @brief The conventions a lookup answers from.
 *
 * Whoever holds it fills it: a service from the refdata service, a test from
 * fixtures. A convention whose address is absent cannot be found by address
 * and is ignored.
 */
struct convention_set final {
    std::vector<refdata::domain::ibor_index_convention> ibor_indices;
    std::vector<refdata::domain::overnight_index_convention> overnight_indices;
    std::vector<refdata::domain::swap_convention> swaps;
    std::vector<refdata::domain::ois_convention> ois_swaps;
};

/// Why no convention was found: the identifier and the reason.
struct lookup_error final {
    std::string reference;
    std::string reason;
};

/// The convention that defines an index: an Ibor index or an overnight one.
using index_definition = std::variant<refdata::domain::ibor_index_convention,
                                      refdata::domain::overnight_index_convention>;

/**
 * @brief The conventions an oresmd identifier's index is defined and used by.
 *
 * An oresmd URI is an identifier: it names one index exactly. What it does not
 * carry are the physical fields a consumer needs to use the index, its
 * calendar, day count and fixing lag, which the conventions hold. This looks
 * them up by the identifier. It is not resolution, which the page Market Data
 * Configuration reserves for mapping a requirement to an identifier under a
 * configuration; here the identifier is already known.
 *
 * A convention states its address in one of two roles, and a question asks
 * for one of them:
 *
 * - A convention that defines an index states the index's family address, with
 *   no tenor: =EUR-EURIBOR= for every Euribor tenor. Asked for the definition
 *   of =EUR-EURIBOR-6M=, the lookup drops the tenor and matches.
 * - A convention that uses an index states that index in full: the swap
 *   convention of =EUR-EURIBOR-6M= names the six-month tenor. It matches the
 *   identifier exactly.
 *
 * Each answer is the one convention that matches, or an error naming the
 * identifier and why: none matches, or more than one does.
 */
class ORES_MARKETDATA_API_EXPORT convention_lookup final {
public:
    explicit convention_lookup(convention_set conventions);

    /// The convention that defines the index @p index addresses.
    [[nodiscard]] std::expected<index_definition, lookup_error>
    index_definition_of(const datum::market_reference& index) const;

    /// The swap convention whose floating leg uses @p index.
    [[nodiscard]] std::expected<refdata::domain::swap_convention, lookup_error>
    swap_convention_of(const datum::market_reference& index) const;

    /// The OIS convention whose floating leg uses @p index.
    [[nodiscard]] std::expected<refdata::domain::ois_convention, lookup_error>
    ois_convention_of(const datum::market_reference& index) const;

private:
    convention_set conventions_;
};

}

#endif
