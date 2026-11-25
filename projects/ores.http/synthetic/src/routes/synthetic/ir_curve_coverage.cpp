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
#include "ores.http/routes/synthetic/ir_curve_coverage.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::synthetic {

namespace {

constexpr std::string_view funding = "FUNDING";
constexpr std::string_view fixing = "fixing";

/** A curve older than two republish firings states a gap: the job runs every fifteen minutes. */
constexpr auto stale_publish = std::chrono::minutes(30);

constexpr std::string_view no_feed_reason = "No synthetic feed holds this currency.";
constexpr std::string_view no_source_reason =
    "The feed keeps a fixed level, because no public source publishes this currency.";
constexpr std::string_view fixed_config_reason =
    "The feed config starts from its fixed level, not from a fixing.";
constexpr std::string_view no_curve_config_reason = "No bootstrap config holds this currency.";
constexpr std::string_view no_publish_reason = "The curve has never been published.";
constexpr std::string_view stale_publish_reason =
    "The latest curve was published more than 30 minutes ago.";
constexpr std::string_view not_live_reason = "The latest set is not the set in use.";

using generation_config = ores::synthetic::domain::ir_curve_generation_config;
using bootstrap_config = ores::refdata::domain::ir_curve_bootstrap_config;
using running_feed = ores::synthetic::messaging::running_feed;

/** A config with no role states the curve the currency discounts off, as the first configs did. */
bool is_funding(const bootstrap_config& config) {
    return config.curve_family_role.empty() || config.curve_family_role == funding;
}

std::string text_of(const boost::uuids::uuid& id) {
    return boost::uuids::to_string(id);
}

std::string text_of(const std::optional<boost::uuids::uuid>& id) {
    return id ? text_of(*id) : std::string{};
}

/** The rows of a list, grouped by the currency each names. */
template <typename T>
std::map<std::string, std::vector<const T*>> group_by_currency(const std::vector<T>& items) {
    std::map<std::string, std::vector<const T*>> by_currency;
    for (const auto& item : items)
        by_currency[item.currency_code].push_back(&item);
    return by_currency;
}

/** The runs of the service, by the source name that names each. */
std::map<std::string, const running_feed*> runs_by_source(const std::vector<running_feed>& runs) {
    std::map<std::string, const running_feed*> out;
    for (const auto& run : runs)
        out.emplace(run.source_name, &run);
    return out;
}

const published_curve_fact* fact_of(const ir_curve_coverage_input& input,
                                    const boost::uuids::uuid& series_id) {
    const auto found = input.published_by_series.find(text_of(series_id));
    return found == input.published_by_series.end() ? nullptr : &found->second;
}

/**
 * @brief The instant a stored publish states, or nothing when the text is not one.
 *
 * The store's text is an ISO instant with a space before the time, which is the
 * shape the platform parser reads from the database.
 */
std::optional<std::chrono::system_clock::time_point> instant_of(const std::string& text) {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_db_string(text);
}

/**
 * @brief The first fact that keeps one published curve from being live, or no gap.
 */
void publish_gap_of(const published_curve_fact* published,
                    std::chrono::system_clock::time_point now,
                    std::string& gap,
                    std::string& reason) {
    if (published == nullptr) {
        gap = "no_publish";
        reason = no_publish_reason;
        return;
    }
    if (!published->state_code.empty() && published->state_code != "published") {
        gap = "not_live";
        reason = std::string(not_live_reason) + " It is " + published->state_code + ".";
        return;
    }
    const auto at = instant_of(published->as_of);
    if (!at || now - *at > stale_publish) {
        gap = "stale_publish";
        reason = stale_publish_reason;
        return;
    }
    gap.clear();
    reason.clear();
}

/**
 * @brief The feed config a currency reads through: the first, by source name, whose
 * run started from a public rate, else the first that is configured to, else the first.
 */
const generation_config* public_config_of(const std::vector<const generation_config*>& configs,
                                          const std::map<std::string, const running_feed*>& runs) {
    std::vector<const generation_config*> ordered = configs;
    std::sort(ordered.begin(), ordered.end(), [](const auto* left, const auto* right) {
        return left->source_name < right->source_name;
    });
    const auto started_from_fixing = [&runs](const generation_config* config) {
        const auto run = runs.find(config->source_name);
        return run != runs.end() && run->second->start && run->second->start->from_fixing;
    };
    if (const auto started = std::ranges::find_if(ordered, started_from_fixing);
        started != ordered.end())
        return *started;
    const auto configured = std::ranges::find_if(
        ordered, [](const generation_config* config) { return config->price_source == fixing; });
    return configured != ordered.end() ? *configured : ordered.front();
}

/**
 * @brief The curve config whose newest publish is the latest, else the first by name.
 */
const bootstrap_config* newest_published_of(const ir_curve_coverage_input& input,
                                            const std::vector<const bootstrap_config*>& configs) {
    std::vector<const bootstrap_config*> ordered = configs;
    std::sort(ordered.begin(), ordered.end(), [](const auto* left, const auto* right) {
        return left->name < right->name;
    });

    const bootstrap_config* best = nullptr;
    std::chrono::system_clock::time_point best_at{};
    for (const auto* config : ordered) {
        const auto* published = fact_of(input, config->output_series_id);
        if (published == nullptr)
            continue;
        const auto at = instant_of(published->as_of);
        if (!at)
            continue;
        if (best == nullptr || *at > best_at) {
            best = config;
            best_at = *at;
        }
    }
    return best != nullptr ? best : ordered.front();
}

/** Every curve of one currency, its funding curve first and each projection after, by name. */
std::vector<ir_curve_of_currency> curves_of(const ir_curve_coverage_input& input,
                                            const std::vector<const bootstrap_config*>& configs) {
    std::map<std::string, std::string> names;
    for (const auto* config : configs)
        names[text_of(config->id)] = config->name;

    std::vector<const bootstrap_config*> ordered = configs;
    std::sort(ordered.begin(), ordered.end(), [](const auto* left, const auto* right) {
        const auto left_funding = is_funding(*left);
        const auto right_funding = is_funding(*right);
        if (left_funding != right_funding)
            return left_funding;
        return left->name < right->name;
    });

    std::vector<ir_curve_of_currency> out;
    out.reserve(ordered.size());
    for (const auto* config : ordered) {
        ir_curve_of_currency curve;
        curve.bootstrap_config_id = text_of(config->id);
        curve.name = config->name;
        curve.role = is_funding(*config) ? std::string(funding) : config->curve_family_role;
        curve.index_tenor = config->index_tenor;
        const auto discounts_off = text_of(config->discount_curve_config_id);
        if (const auto named = names.find(discounts_off);
            !discounts_off.empty() && named != names.end())
            curve.discount_curve_name = named->second;
        const auto* published = fact_of(input, config->output_series_id);
        if (published != nullptr) {
            curve.published_as_of = published->as_of;
            curve.published_state = published->state_code;
        }
        publish_gap_of(published, input.now, curve.gap, curve.gap_reason);
        out.push_back(std::move(curve));
    }
    return out;
}

/** The first gap, in the order a currency falls short of a published curve. */
void gap_of(const generation_config* config,
            const running_feed* run,
            const bootstrap_config* bootstrap,
            const published_curve_fact* published,
            std::chrono::system_clock::time_point now,
            std::string& gap,
            std::string& reason) {
    if (config == nullptr) {
        gap = "no_feed";
        reason = no_feed_reason;
        return;
    }
    if (config->price_source != fixing) {
        gap = "no_source";
        reason = fixed_config_reason;
        return;
    }
    if (run != nullptr && run->start && !run->start->from_fixing) {
        gap = "no_source";
        reason = run->start->note.empty() ? no_source_reason : run->start->note;
        return;
    }
    if (bootstrap == nullptr) {
        gap = "no_curve_config";
        reason = no_curve_config_reason;
        return;
    }
    publish_gap_of(published, now, gap, reason);
}

ir_curve_coverage_row coverage_row(const ir_curve_coverage_input& input,
                                   const std::string& currency,
                                   const std::vector<const generation_config*>& configs,
                                   const std::vector<const bootstrap_config*>& bootstraps) {
    const auto runs = runs_by_source(input.runs);
    const auto* config = configs.empty() ? nullptr : public_config_of(configs, runs);
    const running_feed* run = nullptr;
    if (config != nullptr)
        if (const auto found = runs.find(config->source_name); found != runs.end())
            run = found->second;

    // The currency is covered when its funding curve is: a projection curve that
    // publishes does not stand in for the curve that discounts.
    std::vector<const bootstrap_config*> funding_curves;
    for (const auto* candidate : bootstraps)
        if (is_funding(*candidate))
            funding_curves.push_back(candidate);
    const auto* bootstrap =
        bootstraps.empty() ?
            nullptr :
            newest_published_of(input, funding_curves.empty() ? bootstraps : funding_curves);
    const auto* published =
        bootstrap == nullptr ? nullptr : fact_of(input, bootstrap->output_series_id);

    ir_curve_coverage_row row;
    row.currency_code = currency;
    if (config != nullptr) {
        row.config_id = text_of(config->id);
        row.source_name = config->source_name;
    }
    if (run != nullptr && run->start)
        row.provider_code = run->start->provider_code;
    if (bootstrap != nullptr) {
        row.bootstrap_config_id = text_of(bootstrap->id);
        row.bootstrap_config_name = bootstrap->name;
    }
    if (published != nullptr) {
        row.published_as_of = published->as_of;
        row.published_state = published->state_code;
    }
    gap_of(config, run, bootstrap, published, input.now, row.gap, row.gap_reason);
    row.curves = curves_of(input, bootstraps);
    return row;
}

}

std::vector<ir_curve_coverage_row> ir_curve_coverage(const ir_curve_coverage_input& input) {
    // A run is named by its source name alone, which is also the subject its ticks
    // go out on, so two configs that share a source name share one run, and the
    // service cannot tell them apart either.
    const auto configs = group_by_currency(input.generation_configs);
    const auto bootstraps = group_by_currency(input.bootstrap_configs);

    const std::vector<const generation_config*> no_configs;
    const std::vector<const bootstrap_config*> no_bootstraps;
    std::set<std::string> currencies;
    for (const auto& entry : configs)
        currencies.insert(entry.first);
    for (const auto& entry : bootstraps)
        currencies.insert(entry.first);

    std::vector<ir_curve_coverage_row> out;
    out.reserve(currencies.size());
    for (const auto& currency : currencies) {
        const auto held_configs = configs.find(currency);
        const auto held_bootstraps = bootstraps.find(currency);
        out.push_back(coverage_row(
            input,
            currency,
            held_configs == configs.end() ? no_configs : held_configs->second,
            held_bootstraps == bootstraps.end() ? no_bootstraps : held_bootstraps->second));
    }
    return out;
}

}
