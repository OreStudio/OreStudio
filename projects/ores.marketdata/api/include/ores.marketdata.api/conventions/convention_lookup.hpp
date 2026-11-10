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
#include "ores.refdata.api/domain/cds_convention.hpp"
#include "ores.refdata.api/domain/ibor_index_convention.hpp"
#include "ores.refdata.api/domain/inflation_swap_convention.hpp"
#include "ores.refdata.api/domain/ois_convention.hpp"
#include "ores.refdata.api/domain/overnight_index_convention.hpp"
#include "ores.refdata.api/domain/swap_convention.hpp"
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
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
    std::vector<refdata::domain::cds_convention> cds;
    std::vector<refdata::domain::inflation_swap_convention> inflation_swaps;
};

/// Why the lookup could not answer, kept apart because each is a different
/// fault: the data is missing, or the question is not this lookup's.
enum class lookup_refusal_kind : std::uint8_t {
    /// The address names a datum, not an index.
    addresses_no_index,
    /// The index belongs to a family no convention here defines: this lookup
    /// answers the reducing indexes, Ibor and overnight.
    family_the_lookup_does_not_answer,
    /// No convention states it. Conventions whose own address the grammar
    /// refuses are named in the reason, so a bad address is not read as absent.
    no_match,
    /// More than one convention states it, so the lookup will not choose.
    more_than_one_match
};

/// Why no convention was found: the identifier asked about, and the reason.
struct lookup_error final {
    lookup_refusal_kind kind;
    std::string reference;
    std::string reason;
};

namespace detail {

/**
 * @brief One convention type's addresses.
 *
 * A stored address the grammar reads is held as the reference it names, so the
 * lookup compares values. One the grammar refuses is held aside with its id and
 * the codec's reason, so a refusal can point at the bad row instead of reporting
 * the address absent.
 */
template <typename Convention>
struct answerable final {
    struct stated final {
        datum::market_reference reference;
        Convention convention;
    };

    struct refused final {
        std::string id;
        std::string address;
        std::string reason;
    };

    std::vector<stated> readable;
    std::vector<refused> refused_addresses;
};

}

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
 * A stored address is read into a market reference once, when the lookup is
 * built, and the lookup then matches references by what they name rather than
 * by their text. oresmd gives one datum one spelling and the grammar refuses
 * every other, so a stored address in another spelling is not a reference at
 * all: it is kept aside and named in the refusal of any question it could have
 * answered. That is the difference the parse buys: a bad address is reported
 * as bad rather than as missing.
 *
 * A few families are both: ORE builds USD-SOFR as the overnight index and
 * USD-SOFR-3M as a term rate, from different classes. The index states which
 * one it is by whether it carries a tenor, so the lookup answers with the kind
 * the index states.
 *
 * It falls back to the other kind only when the stated one has no convention
 * for the family, and that fallback is what makes the family address askable:
 * a defining convention states the family with no tenor, so a question about
 * the family carries none either, and must still find an Ibor definition.
 *
 * Each answer is the one convention that matches, or an error naming the
 * identifier and why: none matches, more than one does, or the index is of a
 * family this lookup does not answer for.
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

    /**
     * @brief The CDS convention ORE names @p id.
     *
     * A CDS convention defines no index, and the address it states leaves the
     * reference entity open, so it cannot be found by a trade's curve. ORE
     * names it by id, and a trade states the id, so the lookup does too.
     */
    [[nodiscard]] std::expected<refdata::domain::cds_convention, lookup_error>
    cds_convention_of(std::string_view id) const;

    /// The inflation swap convention whose index is @p index.
    [[nodiscard]] std::expected<refdata::domain::inflation_swap_convention, lookup_error>
    inflation_swap_convention_of(const datum::market_reference& index) const;

private:
    detail::answerable<refdata::domain::ibor_index_convention> ibor_indices_;
    detail::answerable<refdata::domain::overnight_index_convention> overnight_indices_;
    detail::answerable<refdata::domain::swap_convention> swaps_;
    detail::answerable<refdata::domain::ois_convention> ois_swaps_;
    std::vector<refdata::domain::cds_convention> cds_;
    detail::answerable<refdata::domain::inflation_swap_convention> inflation_swaps_;
};

}

#endif
