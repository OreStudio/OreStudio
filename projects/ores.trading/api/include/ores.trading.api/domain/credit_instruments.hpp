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
#ifndef ORES_TRADING_API_DOMAIN_CREDIT_INSTRUMENTS_HPP
#define ORES_TRADING_API_DOMAIN_CREDIT_INSTRUMENTS_HPP

#include "ores.trading.api/domain/credit_instrument.hpp"
#include <array>
#include <chrono>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>

/**
 * @brief The credit family's named constructors.
 *
 * One table holds every credit product, and what distinguishes them is the
 * trade type plus the reference the risk is on. The reference is typed, so a
 * constructor states whether it holds an LEI or an identifier of our own data
 * and cannot leave the two to disagree.
 */
namespace ores::trading::domain {

/**
 * @brief Refuses to construct a product from facts that do not state one.
 */
class credit_product_error final : public std::invalid_argument {
public:
    using std::invalid_argument::invalid_argument;
};

namespace credit_terms {

[[noreturn]] inline void refuse(std::string_view product, std::string_view fact) {
    throw credit_product_error("A " + std::string(product) + " states " + std::string(fact) + ".");
}

inline void
require_text(std::string_view product, std::string_view fact, const std::string& value) {
    if (value.empty())
        refuse(product, fact);
}

/**
 * @brief An LEI is twenty alphanumeric characters; anything else is a
 * different kind of identifier and must say so.
 */
inline void require_lei(std::string_view product, const std::string& value) {
    if (value.size() != 20)
        refuse(product, "an LEI of twenty characters, or an Internal identifier");
    for (const auto c : value) {
        const auto alphanumeric =
            (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
        if (!alphanumeric)
            refuse(product, "an LEI of twenty alphanumeric characters");
    }
}

}

/**
 * @brief The facts every credit product states.
 */
struct credit_common_terms final {
    std::string trade_type_code;
    std::string reference_entity_id_type;
    std::string reference_entity_id;
    std::optional<std::chrono::year_month_day> start_date;
    std::optional<std::chrono::year_month_day> maturity_date;
};

namespace credit_builders {

inline credit_instrument common_row(std::string_view product, const credit_common_terms& terms) {
    using namespace credit_terms;
    require_text(product, "a trade type", terms.trade_type_code);
    require_text(product, "a reference entity identifier", terms.reference_entity_id);
    if (terms.reference_entity_id_type == "LEI")
        require_lei(product, terms.reference_entity_id);
    else if (terms.reference_entity_id_type != "Internal")
        refuse(product, "a reference entity type of LEI or Internal");

    credit_instrument row;
    row.identity.trade_type_code = terms.trade_type_code;
    row.reference_entity_id_type = terms.reference_entity_id_type;
    row.reference_entity_id = terms.reference_entity_id;
    row.start_date = terms.start_date;
    row.maturity_date = terms.maturity_date;
    return row;
}

}

[[nodiscard]] inline credit_instrument
construct_credit_default_swap(const credit_common_terms& terms) {
    return credit_builders::common_row("credit default swap", terms);
}

[[nodiscard]] inline credit_instrument
construct_index_credit_default_swap(const credit_common_terms& terms) {
    return credit_builders::common_row("index credit default swap", terms);
}

[[nodiscard]] inline credit_instrument
construct_index_credit_default_swap_option(const credit_common_terms& terms) {
    return credit_builders::common_row("index credit default swap option", terms);
}

[[nodiscard]] inline credit_instrument
construct_credit_linked_swap(const credit_common_terms& terms) {
    return credit_builders::common_row("credit linked swap", terms);
}

[[nodiscard]] inline credit_instrument
construct_risk_participation_agreement(const credit_common_terms& terms) {
    return credit_builders::common_row("risk participation agreement", terms);
}

[[nodiscard]] inline credit_instrument construct_synthetic_cdo(const credit_common_terms& terms) {
    return credit_builders::common_row("synthetic CDO", terms);
}

}

#endif
