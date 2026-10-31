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
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.marketdata.api/domain/asset_class_authorities.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include <algorithm>
#include <stdexcept>
#include <string_view>
#include <utility>

namespace ores::marketdata::core {

namespace {

// The three values the asset_class_source column allows. "literal" rows carry
// their class in the row; "correlation_operands" rows derive it from the two
// operands of a correlation datum, so they carry no class of their own; and
// "index_authority" rows read it from the index family's own schema row.
constexpr std::string_view k_literal_source = "literal";
constexpr std::string_view k_correlation_operands_source = "correlation_operands";
constexpr std::string_view k_index_authority_source = "index_authority";

constexpr std::string_view k_datum_kind = "datum";
constexpr std::string_view k_index_kind = "index";

constexpr std::string_view k_index_subclass = "index_fixing";

/**
 * ORE names a correlation operand after the factor class it belongs to: an
 * equity index is EQ-RIC:.SPX, an FX rate is FX-GENERIC-USD-EUR, a commodity
 * future is COMM-NYMEX:CL. A rates swap rate is the exception — it carries no
 * prefix and names its currency instead, as in EUR-CMS-10Y.
 */
std::optional<std::string> operand_asset_class(const std::string& operand) {
    if (operand.rfind("EQ-", 0) == 0)
        return "equity";
    if (operand.rfind("FX-", 0) == 0)
        return "fx";
    if (operand.rfind("COMM-", 0) == 0)
        return "commodity";
    if (operand.find("-CMS-") != std::string::npos)
        return "interest_rates";
    return std::nullopt;
}

/**
 * The classes the operands of a correlation name, in operand order and once
 * each, so FX against FX yields one class and not two.
 */
std::vector<std::string> correlation_asset_classes(const std::vector<std::string>& operands) {
    std::vector<std::string> classes;
    for (const auto& operand : operands) {
        const auto code = operand_asset_class(operand);
        if (!code)
            continue;
        if (std::find(classes.begin(), classes.end(), *code) == classes.end())
            classes.push_back(*code);
    }
    return classes;
}

}

classification_key classification_key_of(const datum::market_datum& d) {
    classification_key k;
    k.kind = key_kind::datum;
    k.instrument = datum::oresmd_uri_codec::instrument_spelling(d.type());
    k.quote = datum::oresmd_uri_codec::quote_spelling(d.quote());
    // The correlation row makes both indices identity fields that cannot be
    // none, and market_datum::make refuses a datum without them.
    if (d.type() == datum::instrument_type::correlation) {
        k.operands.push_back(datum::text_of(d.at(datum::field::index1)));
        k.operands.push_back(datum::text_of(d.at(datum::field::index2)));
    }
    return k;
}

classification_key classification_key_of(const datum::market_index& i) {
    classification_key k;
    k.kind = key_kind::index;
    k.instrument = std::string(datum::name_of(i.family()));
    return k;
}

std::string_view name_of(key_kind kind) {
    switch (kind) {
        case key_kind::datum:
            return k_datum_kind;
        case key_kind::index:
            return k_index_kind;
    }
    return k_datum_kind;
}

std::optional<key_kind> key_kind_named(std::string_view name) {
    if (name == k_datum_kind)
        return key_kind::datum;
    if (name == k_index_kind)
        return key_kind::index;
    return std::nullopt;
}

series_classifier::series_classifier(std::vector<domain::series_classification_rule> rules) {
    if (rules.empty())
        throw std::invalid_argument(
            "ores_marketdata_series_classification_rules_tbl is unseeded: no series "
            "classification rules were supplied, so no market series identity could be "
            "classified and every ingest that creates a series would abort.");

    const auto describe = [](const domain::series_classification_rule& rule) {
        return "'" + rule.key_kind + "/" + rule.instrument +
               (rule.quote.empty() ? std::string() : "/" + rule.quote) + "'";
    };

    for (auto& rule : rules) {
        const auto kind = key_kind_named(rule.key_kind);
        if (!kind)
            throw std::invalid_argument("series classification rule " + describe(rule) +
                                        " names the key kind '" + rule.key_kind +
                                        "'; the table allows only '" + std::string(k_datum_kind) +
                                        "' and '" + std::string(k_index_kind) + "'.");

        const bool index = *kind == key_kind::index;

        if (rule.asset_class_source != k_literal_source &&
            rule.asset_class_source != k_correlation_operands_source &&
            rule.asset_class_source != k_index_authority_source)
            throw std::invalid_argument("series classification rule " + describe(rule) +
                                        " names the asset class source '" +
                                        rule.asset_class_source + "'; the table allows only '" +
                                        std::string(k_literal_source) + "', '" +
                                        std::string(k_correlation_operands_source) + "' and '" +
                                        std::string(k_index_authority_source) + "'.");

        if (rule.asset_class_source == k_literal_source && !rule.asset_class_code)
            throw std::invalid_argument("series classification rule " + describe(rule) +
                                        " reads its asset class from the row but carries no "
                                        "asset class code.");

        if (rule.asset_class_source != k_literal_source && rule.asset_class_code)
            throw std::invalid_argument("series classification rule " + describe(rule) +
                                        " derives its asset classes from the identity and also "
                                        "carries the asset class code '" +
                                        *rule.asset_class_code +
                                        "'; a reader would not know which of the two to record.");

        if (index && (rule.asset_class_source != k_index_authority_source ||
                      rule.series_subclass_code != k_index_subclass))
            throw std::invalid_argument(
                "series classification rule " + describe(rule) +
                " is keyed by an index, so it reads its class from the index family and names "
                "the subclass '" +
                std::string(k_index_subclass) + "'.");

        if (!index && rule.asset_class_source == k_index_authority_source)
            throw std::invalid_argument("series classification rule " + describe(rule) +
                                        " is keyed by a datum, so no index family's authority "
                                        "speaks for it.");

        by_kind_[static_cast<std::uint8_t>(*kind)][rule.instrument][rule.quote] = std::move(rule);
    }
}

series_classification series_classifier::classify(const classification_key& key) const {
    const auto result = try_classify(key);
    if (!result)
        throw std::invalid_argument(
            "No classification rule for market series identity: " + std::string(name_of(key.kind)) +
            "/" + key.instrument + (key.quote.empty() ? std::string() : "/" + key.quote));
    return *result;
}

std::optional<series_classification>
series_classifier::try_classify(const classification_key& key) const {
    const auto kind = by_kind_.find(static_cast<std::uint8_t>(key.kind));
    if (kind == by_kind_.end())
        return std::nullopt;

    // An index rule keyed by the empty instrument speaks for every family, so a
    // family with no row of its own takes that one. A datum always names its
    // instrument, so only an index can reach the empty row.
    auto instrument = kind->second.find(key.instrument);
    if (instrument == kind->second.end() && key.kind == key_kind::index)
        instrument = kind->second.find(std::string{});
    if (instrument == kind->second.end())
        return std::nullopt;

    const auto& by_quote = instrument->second;
    auto rule = by_quote.find(key.quote);
    if (rule == by_quote.end())
        rule = by_quote.find(std::string{});
    if (rule == by_quote.end())
        return std::nullopt;

    const auto& row = rule->second;
    if (row.asset_class_source == k_correlation_operands_source)
        return series_classification{correlation_asset_classes(key.operands),
                                     row.series_subclass_code};

    if (row.asset_class_source == k_index_authority_source) {
        // The index reads its class from its own schema row: the family's asset
        // authority names the market-data namespace the index is written in, and
        // the catalogue maps that authority onto a refdata class. A family that
        // maps to no class leaves the list empty rather than mis-classing the
        // series.
        const auto family = datum::index_family_named(key.instrument);
        if (!family)
            return series_classification{{}, row.series_subclass_code};
        const auto code =
            domain::asset_class_for_authority(datum::name_of(datum::index_row_of(*family).asset));
        if (!code)
            return series_classification{{}, row.series_subclass_code};
        return series_classification{{std::string(*code)}, row.series_subclass_code};
    }

    return series_classification{{*row.asset_class_code}, row.series_subclass_code};
}

std::vector<std::string> series_classifier::known_instruments(key_kind kind) const {
    std::vector<std::string> instruments;
    const auto found = by_kind_.find(static_cast<std::uint8_t>(kind));
    if (found == by_kind_.end())
        return instruments;
    instruments.reserve(found->second.size());
    for (const auto& [instrument, by_quote] : found->second)
        instruments.push_back(instrument);
    std::sort(instruments.begin(), instruments.end());
    return instruments;
}

}
