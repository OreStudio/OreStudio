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
#ifndef ORES_MARKETDATA_SERVICE_APP_COMPOSITE_ENGINE_HPP
#define ORES_MARKETDATA_SERVICE_APP_COMPOSITE_ENGINE_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.marketdata.api/domain/composite.hpp"
#include "ores.marketdata.api/domain/composite_input.hpp"
#include "ores.marketdata.api/domain/subscription.hpp"
#include "ores.marketdata.service/export.hpp"
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief A rule that derives the value of a composite from the values of its inputs.
 *
 * The values come in the order of the positions of the inputs, and only the inputs that have a
 * value are in the list. A rule returns nothing when it cannot derive a value from the list.
 * A new rule is a new class and a new code, and the composite names it by the code, so the model
 * needs no change.
 */
class ORES_MARKETDATA_SERVICE_EXPORT composite_rule {
public:
    virtual ~composite_rule() = default;

    /// The code that a composite names the rule by.
    [[nodiscard]] virtual std::string_view code() const = 0;

    /// The value derived from @p values, in the order of the positions of the inputs.
    [[nodiscard]] virtual std::optional<double>
    evaluate(const std::vector<double>& values) const = 0;
};

/// The rule of @p code, or nothing for a code that names no rule. The rules are pass_through,
/// which takes the first input that has a value, average, and median.
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::unique_ptr<composite_rule>
make_composite_rule(std::string_view code);

/// The value of a composite, or the reason it has none.
struct composite_value {
    std::optional<double> value;
    /// Why there is no value. Empty when there is one.
    std::string reason;
};

/// The latest value of an input, by provider and item.
using input_values = std::map<std::pair<std::string, std::string>, double>;

/**
 * @brief The providers of the inputs of a composite on which a party lacks the derived right.
 *
 * The party needs the derived right on the provider of every input. The list is sorted and holds
 * a provider once.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::vector<std::string>
missing_derived_rights(const std::vector<domain::composite_input>& inputs,
                       const std::vector<domain::subscription>& subscriptions_of_party);

/**
 * @brief Derives the value of @p composite for a party, or says why there is none.
 *
 * The reasons are an unknown rule, a missing derived right on an input provider, and no input
 * with a value. A composite with a missing right gets no value, however many inputs have one.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT composite_value
evaluate_composite(const domain::composite& composite,
                   const std::vector<domain::composite_input>& inputs,
                   const std::vector<domain::subscription>& subscriptions_of_party,
                   const input_values& latest);

/**
 * @brief A function from the item of a composite to the items of its inputs, in the order of
 * their positions, for the demand policy.
 *
 * The inputs count as demand on their own providers, so the charge components of those providers
 * see the use of the composite. An item that is no composite has no inputs.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT
    std::function<std::vector<std::string>(const std::string&)>
    composite_input_resolver(const std::vector<domain::composite>& composites,
                             const std::vector<domain::composite_input>& inputs);

}

#endif
