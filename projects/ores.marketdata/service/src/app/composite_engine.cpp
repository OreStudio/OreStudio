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
#include "ores.marketdata.service/app/composite_engine.hpp"
#include <algorithm>
#include <cmath>
#include <numeric>
#include <set>

namespace ores::marketdata::service::app {

namespace {

class pass_through_rule final : public composite_rule {
public:
    std::string_view code() const override {
        return "pass_through";
    }
    std::optional<double> evaluate(const std::vector<double>& values) const override {
        if (values.empty())
            return std::nullopt;
        return values.front();
    }
};

class average_rule final : public composite_rule {
public:
    std::string_view code() const override {
        return "average";
    }
    std::optional<double> evaluate(const std::vector<double>& values) const override {
        if (values.empty())
            return std::nullopt;
        return std::accumulate(values.begin(), values.end(), 0.0) /
               static_cast<double>(values.size());
    }
};

class median_rule final : public composite_rule {
public:
    std::string_view code() const override {
        return "median";
    }
    std::optional<double> evaluate(const std::vector<double>& values) const override {
        if (values.empty())
            return std::nullopt;
        auto sorted = values;
        std::ranges::sort(sorted);
        const auto n = sorted.size();
        if (n % 2 == 1)
            return sorted[n / 2];
        return (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0;
    }
};

bool holds_derived(const std::vector<domain::subscription>& subscriptions,
                   const std::string& provider) {
    return std::ranges::any_of(subscriptions, [&](const domain::subscription& s) {
        return s.provider_code == provider && s.derived;
    });
}

std::vector<const domain::composite_input*>
in_position_order(const std::vector<domain::composite_input>& inputs) {
    std::vector<const domain::composite_input*> ordered;
    for (const auto& i : inputs)
        ordered.push_back(&i);
    std::ranges::stable_sort(
        ordered, [](const auto* a, const auto* b) { return a->position < b->position; });
    return ordered;
}

}

std::unique_ptr<composite_rule> make_composite_rule(std::string_view code) {
    if (code == "pass_through")
        return std::make_unique<pass_through_rule>();
    if (code == "average")
        return std::make_unique<average_rule>();
    if (code == "median")
        return std::make_unique<median_rule>();
    return nullptr;
}

std::vector<std::string>
missing_derived_rights(const std::vector<domain::composite_input>& inputs,
                       const std::vector<domain::subscription>& subscriptions_of_party) {
    std::set<std::string> missing;
    for (const auto& input : inputs) {
        if (!holds_derived(subscriptions_of_party, input.provider_code))
            missing.insert(input.provider_code);
    }
    return {missing.begin(), missing.end()};
}

composite_value evaluate_composite(const domain::composite& composite,
                                   const std::vector<domain::composite_input>& inputs,
                                   const std::vector<domain::subscription>& subscriptions_of_party,
                                   const input_values& latest) {
    composite_value out;

    const auto rule = make_composite_rule(composite.rule_code);
    if (!rule) {
        out.reason = "The rule " + composite.rule_code + " is unknown.";
        return out;
    }

    const auto missing = missing_derived_rights(inputs, subscriptions_of_party);
    if (!missing.empty()) {
        std::string providers;
        for (const auto& provider : missing)
            providers += (providers.empty() ? "" : ", ") + provider;
        out.reason =
            "The party lacks the derived right on the provider of an input: " + providers + ".";
        return out;
    }

    std::vector<double> values;
    for (const auto* input : in_position_order(inputs)) {
        const auto found = latest.find({input->provider_code, input->oresmd_uri});
        // A value that is not a number is no value, so no rule sees one.
        if (found != latest.end() && std::isfinite(found->second))
            values.push_back(found->second);
    }
    out.value = rule->evaluate(values);
    if (!out.value)
        out.reason = "No input has a value.";
    return out;
}

std::function<std::vector<std::string>(const std::string&)>
composite_input_resolver(const std::vector<domain::composite>& composites,
                         const std::vector<domain::composite_input>& inputs) {
    std::map<std::string, std::vector<std::string>> by_item;
    for (const auto& composite : composites) {
        std::vector<domain::composite_input> mine;
        for (const auto& input : inputs)
            if (input.composite_id == composite.id)
                mine.push_back(input);
        auto& items = by_item[composite.oresmd_uri];
        for (const auto* input : in_position_order(mine))
            items.push_back(input->oresmd_uri);
    }
    return [by_item = std::move(by_item)](const std::string& uri) {
        const auto found = by_item.find(uri);
        return found == by_item.end() ? std::vector<std::string>{} : found->second;
    };
}

}
