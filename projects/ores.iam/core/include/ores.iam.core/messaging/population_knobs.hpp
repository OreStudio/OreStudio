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
#ifndef ORES_IAM_CORE_MESSAGING_POPULATION_KNOBS_HPP
#define ORES_IAM_CORE_MESSAGING_POPULATION_KNOBS_HPP

#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.iam.core/messaging/provision_step_arguments.hpp"
#include <algorithm>
#include <cstddef>
#include <format>
#include <functional>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief The population a run states by numbers rather than by seeded rows.
 *
 * A profile's generate_trades step states one row per config, each with the
 * count it books. That is a recipe, not a size: an operator who wants a
 * different book would edit the seeded rows. The knobs let the run state the
 * size it wants — a total and a percentage per class — and the step turns those
 * into the count each config states.
 *
 * The arithmetic is here rather than in a screen, because the screen shows what
 * the server will do and not an answer of its own making.
 */
struct population_knobs final {
    /// The total trades the run writes, or zero when the run states none.
    std::uint32_t total = 0;
    /// The percentage each asset class takes of that total, by asset class code.
    std::map<std::string, std::uint32_t> percent;

    [[nodiscard]] bool states_any() const {
        return total > 0 || !percent.empty();
    }
};

namespace detail {

/// The asset class a knob's name names, when the name is one.
inline std::optional<std::string> class_of_parameter(std::string_view name) {
    constexpr std::string_view prefix = "population_";
    constexpr std::string_view suffix = "_percent";
    if (!name.starts_with(prefix) || !name.ends_with(suffix) || name.size() <= prefix.size() + suffix.size())
        return std::nullopt;
    return std::string(name.substr(prefix.size(), name.size() - prefix.size() - suffix.size()));
}

inline std::uint32_t read_count(const std::string& name, const std::string& value) {
    try {
        std::size_t at = 0;
        const auto parsed = std::stoul(value, &at);
        if (at != value.size())
            throw std::invalid_argument("trailing characters");
        return static_cast<std::uint32_t>(parsed);
    } catch (const std::exception&) {
        throw std::runtime_error("The population parameter '" + name + "' states '" + value +
                                 "', which is not a whole number.");
    }
}

}

/**
 * @brief Reads the knobs a run's parameter values state.
 *
 * A run that states no total states no knobs, and the step keeps the counts its
 * recipe states. A run that states a total but no percentage takes the class
 * mix its recipe already states, which is what makes the size a knob on its own.
 */
[[nodiscard]] inline population_knobs
read_population_knobs(const std::vector<workflow::provision_tenant_parameter>& parameters) {
    population_knobs knobs;
    for (const auto& parameter : parameters) {
        if (parameter.name == "population_size") {
            if (!parameter.value.empty())
                knobs.total = detail::read_count(parameter.name, parameter.value);
            continue;
        }
        if (const auto asset_class = detail::class_of_parameter(parameter.name)) {
            if (!parameter.value.empty())
                knobs.percent.emplace(*asset_class,
                                      detail::read_count(parameter.name, parameter.value));
        }
    }
    return knobs;
}

/**
 * @brief States each config's count from the knobs.
 *
 * A class's trades are spread over the configs of that class, and no config may
 * state fewer trades than the codes its planner cycles, because the planner
 * draws them by position and would drop the codes past its slot count.
 *
 * @param code_count The codes a class's planner cycles, injected so that this
 *        header needs no planner of its own.
 * @throws std::runtime_error when the mix does not sum to a hundred, when a
 *         class's share cannot hold its codes, or when the knobs name a class
 *         the run books nothing for.
 */
inline void apply_population_knobs(std::vector<trade_generation_assignment>& assignments,
                                   const population_knobs& knobs,
                                   const std::function<std::size_t(const std::string&)>&
                                       code_count) {
    if (!knobs.states_any())
        return;

    std::map<std::string, std::vector<trade_generation_assignment*>> by_class;
    for (auto& assignment : assignments)
        by_class[assignment.asset_class_code].push_back(&assignment);

    std::uint32_t stated = 0;
    for (const auto& [asset_class, percent] : knobs.percent)
        stated += percent;

    if (knobs.percent.empty() && knobs.total > 0) {
        /*
         * A size with no share states no mix: the counts the recipe states are
         * the book the operator asked to make bigger, and scaling them by a
         * guess would be a different book than the one they run.
         */
    } else {
        if (stated == 0)
            throw std::runtime_error(
                "The population mix states no share at all, so no class would take any of it.");
        for (const auto& [asset_class, percent] : knobs.percent)
            if (!by_class.contains(asset_class))
                throw std::runtime_error("The population mix states '" + asset_class +
                                         "', and the run books no trade of that class.");
    }

    /*
     * The shares are shares and not percentages: they are scaled to the total
     * the run states, so a mix that sums to ninety-seven or to a thousand is the
     * same mix. The largest-remainder rule gives the class whose remainder is
     * greatest the trade a rounded-down allocation leaves over, so the classes
     * together state exactly the total.
     */
    const auto total_shares = [&knobs] {
        std::uint32_t sum = 0;
        for (const auto& [asset_class, share] : knobs.percent)
            sum += share;
        return sum;
    }();

    std::map<std::string, std::uint32_t> shares;
    std::map<std::string, std::uint64_t> remainders;
    std::uint32_t allocated = 0;
    for (const auto& [asset_class, configs] : by_class) {
        const auto held = knobs.percent.find(asset_class);
        if (held == knobs.percent.end())
            continue;
        const auto numerator = static_cast<std::uint64_t>(knobs.total) * held->second;
        shares.emplace(asset_class, static_cast<std::uint32_t>(numerator / total_shares));
        remainders.emplace(asset_class, numerator % total_shares);
        allocated += shares.at(asset_class);
    }
    auto spare = knobs.total > allocated ? knobs.total - allocated : 0;
    while (spare > 0) {
        auto largest = shares.end();
        for (auto candidate = shares.begin(); candidate != shares.end(); ++candidate)
            if (candidate->second > 0 && (largest == shares.end() ||
                                          remainders.at(candidate->first) >
                                              remainders.at(largest->first)))
                largest = candidate;
        if (largest == shares.end())
            break;
        ++largest->second;
        remainders[largest->first] = 0;
        --spare;
    }

    for (const auto& [asset_class, configs] : by_class) {
        const auto held = knobs.percent.find(asset_class);
        /*
         * A run that states a total and no mix keeps the counts its recipe
         * states, so the class is left alone.
         */
        if (held == knobs.percent.end())
            continue;

        const auto percent = held->second;
        if (percent == 0) {
            for (auto* config : configs)
                config->trades_per_book = 0;
            continue;
        }

        const auto target = shares.at(asset_class);
        const auto codes = static_cast<std::uint32_t>(code_count(asset_class));
        const auto floor = codes * static_cast<std::uint32_t>(configs.size());
        if (target < floor) {
            /*
             * The smallest total whose share of this class holds its codes. A
             * refusal that names it is one the operator can act on.
             */
            const auto smallest = static_cast<std::uint64_t>(
                (static_cast<std::uint64_t>(floor) * total_shares + percent - 1) / percent);
            throw std::runtime_error(
                std::format("The population of {} trades gives '{}' {} trades, and its {} configs "
                            "state at least one trade per code it cycles, which is {}. A total of "
                            "{} is the smallest that holds them.",
                            knobs.total,
                            asset_class,
                            target,
                            configs.size(),
                            floor,
                            smallest));
        }

        /*
         * The remainder goes to the first configs, so the class states exactly
         * its share rather than a rounded-down version of it.
         */
        const auto base = target / static_cast<std::uint32_t>(configs.size());
        const auto spare = target % static_cast<std::uint32_t>(configs.size());
        for (std::size_t i = 0; i < configs.size(); ++i) {
            const auto extra = i < spare ? 1 : 0;
            configs[i]->trades_per_book = base + static_cast<std::uint32_t>(extra);
        }
    }
}

}

#endif
