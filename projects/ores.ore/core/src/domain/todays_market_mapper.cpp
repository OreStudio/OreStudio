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
#include "ores.ore.core/domain/todays_market_mapper.hpp"
#include "ores.ore.core/domain/ore_code_tables.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

using analytics::domain::todays_market_collection;
using analytics::domain::todays_market_configuration;
using analytics::domain::todays_market_configuration_binding;
using analytics::domain::todays_market_entry;

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

boost::uuids::uuid new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

template <typename T>
void set_audit(T& r) {
    r.modified_by = std::string(audit_modified_by);
    r.performed_by = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary = std::string(audit_commentary);
}

// The reference text of an entry is its base subobject. Every entry except
// SwapIndex has one; SwapIndex carries its value in a nested child instead.
template <typename T>
std::string base_text(const T& e) {
    return static_cast<const xsd::string&>(e);
}

// The second key of the collections that have one, and nothing for the rest.
const auto no_second_key = [](const auto&) {
    return std::optional<std::string>{};
};

std::optional<std::string> collection_id_of(const auto& wrapper) {
    if (!wrapper.id)
        return std::nullopt;
    return std::string(static_cast<const xsd::string&>(*wrapper.id));
}

// The key is absent only where the schema makes it optional. Anywhere else a
// row without one cannot be written back as a valid document.
const std::string& required_key(const todays_market_entry& r) {
    if (!r.key_value)
        throw std::runtime_error("todays_market_mapper: entry " + r.target +
                                 " has no key value, which its collection requires");
    return *r.key_value;
}

boost::uuids::uuid add_collection(ores::analytics::domain::todays_market_document& out,
                                  const boost::uuids::uuid& config_id,
                                  const char* name,
                                  std::optional<std::string> collection_id,
                                  int& collection_position) {
    todays_market_collection c;
    c.id = new_uuid();
    c.todays_market_config_id = config_id;
    c.collection = name;
    c.collection_id = std::move(collection_id);
    c.position = collection_position++;
    set_audit(c);
    out.collections.push_back(c);
    return c.id;
}

// One collection element becomes a collection row and one entry row per entry.
template <typename Wrapper, typename EntryT, typename K1, typename K2, typename Tg>
void map_collection(ores::analytics::domain::todays_market_document& out,
                    const boost::uuids::uuid& config_id,
                    const char* name,
                    int& collection_position,
                    const xsd::vector<Wrapper>& wrappers,
                    xsd::vector<EntryT> Wrapper::* entries,
                    K1 key1,
                    K2 key2,
                    Tg target) {
    for (const auto& w : wrappers) {
        const auto collection_id =
            add_collection(out, config_id, name, collection_id_of(w), collection_position);

        int position = 0;
        for (const auto& e : w.*entries) {
            todays_market_entry row;
            row.id = new_uuid();
            row.todays_market_config_id = config_id;
            row.todays_market_collection_id = collection_id;
            row.key_value = key1(e);
            row.key_value_2 = key2(e);
            row.target = target(e);
            row.position = position++;
            set_audit(row);
            out.entries.push_back(std::move(row));
        }
    }
}

// The reverse: one collection row and its entry rows become one element.
template <typename Wrapper, typename EntryT, typename Build>
void build_collection(todaysmarket& doc,
                      xsd::vector<Wrapper> todaysmarket::* member,
                      xsd::vector<EntryT> Wrapper::* entries,
                      const todays_market_collection& c,
                      const std::vector<const todays_market_entry*>& rows,
                      Build build) {
    Wrapper w;
    if (c.collection_id)
        w.id = *c.collection_id;
    for (const auto* r : rows)
        (w.*entries).push_back(build(*r));
    (doc.*member).push_back(std::move(w));
}

}

ores::analytics::domain::todays_market_document todays_market_mapper::map(const todaysmarket& v) {
    ores::analytics::domain::todays_market_document mapped;

    auto& config = mapped.config;
    config.id = new_uuid();
    config.name = "TodaysMarket";
    config.description = "Imported from ORE XML";
    config.config_variant = "";
    set_audit(config);

    int configuration_position = 0;
    for (const auto& c : v.Configuration) {
        todays_market_configuration row;
        row.id = new_uuid();
        row.todays_market_config_id = config.id;
        row.configuration_id = c.id;
        row.position = configuration_position++;
        set_audit(row);
        mapped.configurations.push_back(row);

        int binding_position = 0;
        const auto add = [&](const char* name, const auto& field) {
            if (!field)
                return;
            todays_market_configuration_binding b;
            b.id = new_uuid();
            b.todays_market_configuration_id = row.id;
            b.collection = name;
            b.reference = static_cast<const xsd::string&>(*field);
            b.position = binding_position++;
            set_audit(b);
            mapped.bindings.push_back(b);
        };
        add("YieldCurves", c.YieldCurvesId);
        add("DiscountingCurves", c.DiscountingCurvesId);
        add("IndexForwardingCurves", c.IndexForwardingCurvesId);
        add("SwapIndexCurves", c.SwapIndexCurvesId);
        add("ZeroInflationIndexCurves", c.ZeroInflationIndexCurvesId);
        add("ZeroInflationCapFloorVolatilities", c.ZeroInflationCapFloorVolatilitiesId);
        add("YYInflationIndexCurves", c.YYInflationIndexCurvesId);
        add("FxSpots", c.FxSpotsId);
        add("BaseCorrelations", c.BaseCorrelationsId);
        add("FxVolatilities", c.FxVolatilitiesId);
        add("SwaptionVolatilities", c.SwaptionVolatilitiesId);
        add("YieldVolatilities", c.YieldVolatilitiesId);
        add("CapFloorVolatilities", c.CapFloorVolatilitiesId);
        add("CDSVolatilities", c.CDSVolatilitiesId);
        add("DefaultCurves", c.DefaultCurvesId);
        add("YYInflationCapFloorVolatilities", c.YYInflationCapFloorVolatilitiesId);
        add("EquityCurves", c.EquityCurvesId);
        add("EquityVolatilities", c.EquityVolatilitiesId);
        add("Securities", c.SecuritiesId);
        add("CommodityCurves", c.CommodityCurvesId);
        add("CommodityVolatilities", c.CommodityVolatilitiesId);
        add("Correlations", c.CorrelationsId);
        add("BondFutureVolatilities", c.BondFutureVolatilitiesId);
        add("IntradayPowerPriceCurves", c.IntradayPowerPriceCurvesId);
    }

    int cp = 0;
    const auto by_name = [](const auto& e) {
        return std::string(e.name);
    };
    map_collection(mapped,
                   config.id,
                   "YieldCurves",
                   cp,
                   v.YieldCurves,
                   &yieldCurvesType::YieldCurve,
                   by_name,
                   no_second_key,
                   base_text<yieldCurvesType_YieldCurve_t>);
    map_collection(mapped,
                   config.id,
                   "IndexForwardingCurves",
                   cp,
                   v.IndexForwardingCurves,
                   &indexForwardingCurvesType::Index,
                   by_name,
                   no_second_key,
                   base_text<indexForwardingCurvesType_Index_t>);
    map_collection(mapped,
                   config.id,
                   "ZeroInflationIndexCurves",
                   cp,
                   v.ZeroInflationIndexCurves,
                   &zeroInflationIndexCurvesType::ZeroInflationIndexCurve,
                   by_name,
                   no_second_key,
                   base_text<zeroInflationIndexCurvesType_ZeroInflationIndexCurve_t>);
    map_collection(mapped,
                   config.id,
                   "YYInflationIndexCurves",
                   cp,
                   v.YYInflationIndexCurves,
                   &yyInflationIndexCurvesType::YYInflationIndexCurve,
                   by_name,
                   no_second_key,
                   base_text<yyInflationIndexCurvesType_YYInflationIndexCurve_t>);
    map_collection(mapped,
                   config.id,
                   "YieldVolatilities",
                   cp,
                   v.YieldVolatilities,
                   &yieldVolatilitiesType::YieldVolatility,
                   by_name,
                   no_second_key,
                   base_text<yieldVolatilitiesType_YieldVolatility_t>);
    map_collection(mapped,
                   config.id,
                   "CDSVolatilities",
                   cp,
                   v.CDSVolatilities,
                   &cdsVolatilitiesType::CDSVolatility,
                   by_name,
                   no_second_key,
                   base_text<cdsVolatilitiesType_CDSVolatility_t>);
    map_collection(mapped,
                   config.id,
                   "DefaultCurves",
                   cp,
                   v.DefaultCurves,
                   &defaultCurvesType::DefaultCurve,
                   by_name,
                   no_second_key,
                   base_text<defaultCurvesType_DefaultCurve_t>);
    map_collection(mapped,
                   config.id,
                   "EquityCurves",
                   cp,
                   v.EquityCurves,
                   &equityCurvesType::EquityCurve,
                   by_name,
                   no_second_key,
                   base_text<equityCurvesType_EquityCurve_t>);
    map_collection(mapped,
                   config.id,
                   "EquityVolatilities",
                   cp,
                   v.EquityVolatilities,
                   &equityVolatilitiesType::EquityVolatility,
                   by_name,
                   no_second_key,
                   base_text<equityVolatilitiesType_EquityVolatility_t>);
    map_collection(mapped,
                   config.id,
                   "Securities",
                   cp,
                   v.Securities,
                   &securitiesType::Security,
                   by_name,
                   no_second_key,
                   base_text<securitiesType_Security_t>);
    map_collection(mapped,
                   config.id,
                   "BaseCorrelations",
                   cp,
                   v.BaseCorrelations,
                   &baseCorrelationsType::BaseCorrelation,
                   by_name,
                   no_second_key,
                   base_text<baseCorrelationsType_BaseCorrelation_t>);
    map_collection(mapped,
                   config.id,
                   "CommodityCurves",
                   cp,
                   v.CommodityCurves,
                   &commodityCurvesType::CommodityCurve,
                   by_name,
                   no_second_key,
                   base_text<commodityCurvesType_CommodityCurve_t>);
    map_collection(mapped,
                   config.id,
                   "CommodityVolatilities",
                   cp,
                   v.CommodityVolatilities,
                   &commodityVolatilitiesType::CommodityVolatility,
                   by_name,
                   no_second_key,
                   base_text<commodityVolatilitiesType_CommodityVolatility_t>);
    map_collection(mapped,
                   config.id,
                   "Correlations",
                   cp,
                   v.Correlations,
                   &correlationsType::Correlation,
                   by_name,
                   no_second_key,
                   base_text<correlationsType_Correlation_t>);
    map_collection(mapped,
                   config.id,
                   "BondFutureVolatilities",
                   cp,
                   v.BondFutureVolatilities,
                   &bondFutureVolatilitiesType::BondFutureVolatility,
                   by_name,
                   no_second_key,
                   base_text<bondFutureVolatilitiesType_BondFutureVolatility_t>);
    map_collection(mapped,
                   config.id,
                   "IntradayPowerPriceCurves",
                   cp,
                   v.IntradayPowerPriceCurves,
                   &intradayPowerPriceCurvesType::IntradayPowerPriceCurve,
                   by_name,
                   no_second_key,
                   base_text<intradayPowerPriceCurvesType_IntradayPowerPriceCurve_t>);
    map_collection(
        mapped,
        config.id,
        "DiscountingCurves",
        cp,
        v.DiscountingCurves,
        &discountCurvesType::DiscountingCurve,
        [](const auto& e) { return std::string(e.currency); },
        no_second_key,
        base_text<discountCurvesType_DiscountingCurve_t>);
    map_collection(
        mapped,
        config.id,
        "FxSpots",
        cp,
        v.FxSpots,
        &fxSpotsType::FxSpot,
        [](const auto& e) { return std::string(e.pair); },
        no_second_key,
        base_text<fxSpotsType_FxSpot_t>);
    map_collection(
        mapped,
        config.id,
        "FxVolatilities",
        cp,
        v.FxVolatilities,
        &fxVolatilitiesType::FxVolatility,
        [](const auto& e) { return std::string(e.pair); },
        no_second_key,
        base_text<fxVolatilitiesType_FxVolatility_t>);
    map_collection(
        mapped,
        config.id,
        "SwaptionVolatilities",
        cp,
        v.SwaptionVolatilities,
        &swaptionVolatilitiesType::SwaptionVolatility,
        [](const auto& e) {
            return e.key ? std::optional<std::string>(std::string(*e.key)) : std::nullopt;
        },
        [](const auto& e) {
            return e.currency ? std::optional<std::string>(to_string(*e.currency)) : std::nullopt;
        },
        base_text<swaptionVolatilitiesType_SwaptionVolatility_t>);
    map_collection(
        mapped,
        config.id,
        "CapFloorVolatilities",
        cp,
        v.CapFloorVolatilities,
        &capFloorVolatilitiesType::CapFloorVolatility,
        [](const auto& e) {
            return e.key ? std::optional<std::string>(std::string(*e.key)) : std::nullopt;
        },
        [](const auto& e) {
            return e.currency ? std::optional<std::string>(to_string(*e.currency)) : std::nullopt;
        },
        base_text<capFloorVolatilitiesType_CapFloorVolatility_t>);
    map_collection(
        mapped,
        config.id,
        "ZeroInflationCapFloorVolatilities",
        cp,
        v.ZeroInflationCapFloorVolatilities,
        &zeroInflationCapFloorVolatilitiesType::ZeroInflationCapFloorVolatility,
        by_name,
        no_second_key,
        base_text<zeroInflationCapFloorVolatilitiesType_ZeroInflationCapFloorVolatility_t>);
    map_collection(mapped,
                   config.id,
                   "YYInflationCapFloorVolatilities",
                   cp,
                   v.YYInflationCapFloorVolatilities,
                   &yyInflationCapFloorVolatilitiesType::YYInflationCapFloorVolatility,
                   by_name,
                   no_second_key,
                   base_text<yyInflationCapFloorVolatilitiesType_YYInflationCapFloorVolatility_t>);

    // SwapIndexCurves is the exception: its entry has no reference text, and its
    // value is the nested Discounting child.
    for (const auto& w : v.SwapIndexCurves) {
        const auto collection_id =
            add_collection(mapped, config.id, "SwapIndexCurves", collection_id_of(w), cp);

        int position = 0;
        for (const auto& e : w.SwapIndex) {
            todays_market_entry row;
            row.id = new_uuid();
            row.todays_market_config_id = config.id;
            row.todays_market_collection_id = collection_id;
            row.key_value = e.name;
            row.target = "";
            row.discounting = e.Discounting;
            row.position = position++;
            set_audit(row);
            mapped.entries.push_back(std::move(row));
        }
    }

    return mapped;
}

todaysmarket
todays_market_mapper::reverse(const ores::analytics::domain::todays_market_document& v) {
    todaysmarket doc;

    auto collections = v.collections;
    std::sort(collections.begin(), collections.end(), [](const auto& a, const auto& b) {
        return a.position < b.position;
    });

    const auto rows_for = [&v](const boost::uuids::uuid& collection_id) {
        std::vector<const todays_market_entry*> rows;
        for (const auto& r : v.entries) {
            if (r.todays_market_collection_id == collection_id)
                rows.push_back(&r);
        }
        std::sort(rows.begin(), rows.end(), [](const auto* a, const auto* b) {
            return a->position < b->position;
        });
        return rows;
    };

    for (const auto& c : collections) {
        const auto rows = rows_for(c.id);

        if (c.collection == "YieldCurves") {
            build_collection(doc,
                             &todaysmarket::YieldCurves,
                             &yieldCurvesType::YieldCurve,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 yieldCurvesType_YieldCurve_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "IndexForwardingCurves") {
            build_collection(doc,
                             &todaysmarket::IndexForwardingCurves,
                             &indexForwardingCurvesType::Index,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 indexForwardingCurvesType_Index_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "DiscountingCurves") {
            build_collection(doc,
                             &todaysmarket::DiscountingCurves,
                             &discountCurvesType::DiscountingCurve,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 discountCurvesType_DiscountingCurve_t e;
                                 e.currency = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "SwapIndexCurves") {
            build_collection(doc,
                             &todaysmarket::SwapIndexCurves,
                             &swapIndexCurvesType::SwapIndex,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 swapIndexCurvesType_SwapIndex_t e;
                                 e.name = required_key(r);
                                 e.Discounting = r.discounting.value_or("");
                                 return e;
                             });
        } else if (c.collection == "ZeroInflationIndexCurves") {
            build_collection(doc,
                             &todaysmarket::ZeroInflationIndexCurves,
                             &zeroInflationIndexCurvesType::ZeroInflationIndexCurve,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 zeroInflationIndexCurvesType_ZeroInflationIndexCurve_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "YYInflationIndexCurves") {
            build_collection(doc,
                             &todaysmarket::YYInflationIndexCurves,
                             &yyInflationIndexCurvesType::YYInflationIndexCurve,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 yyInflationIndexCurvesType_YYInflationIndexCurve_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "FxSpots") {
            build_collection(doc,
                             &todaysmarket::FxSpots,
                             &fxSpotsType::FxSpot,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 fxSpotsType_FxSpot_t e;
                                 e.pair = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "FxVolatilities") {
            build_collection(doc,
                             &todaysmarket::FxVolatilities,
                             &fxVolatilitiesType::FxVolatility,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 fxVolatilitiesType_FxVolatility_t e;
                                 e.pair = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "SwaptionVolatilities") {
            build_collection(doc,
                             &todaysmarket::SwaptionVolatilities,
                             &swaptionVolatilitiesType::SwaptionVolatility,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 swaptionVolatilitiesType_SwaptionVolatility_t e;
                                 if (r.key_value)
                                     e.key = *r.key_value;
                                 if (r.key_value_2)
                                     e.currency = parse_currency_code(*r.key_value_2);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "CapFloorVolatilities") {
            build_collection(doc,
                             &todaysmarket::CapFloorVolatilities,
                             &capFloorVolatilitiesType::CapFloorVolatility,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 capFloorVolatilitiesType_CapFloorVolatility_t e;
                                 if (r.key_value)
                                     e.key = *r.key_value;
                                 if (r.key_value_2)
                                     e.currency = parse_currency_code(*r.key_value_2);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "YieldVolatilities") {
            build_collection(doc,
                             &todaysmarket::YieldVolatilities,
                             &yieldVolatilitiesType::YieldVolatility,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 yieldVolatilitiesType_YieldVolatility_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "CDSVolatilities") {
            build_collection(doc,
                             &todaysmarket::CDSVolatilities,
                             &cdsVolatilitiesType::CDSVolatility,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 cdsVolatilitiesType_CDSVolatility_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "DefaultCurves") {
            build_collection(doc,
                             &todaysmarket::DefaultCurves,
                             &defaultCurvesType::DefaultCurve,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 defaultCurvesType_DefaultCurve_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "YYInflationCapFloorVolatilities") {
            build_collection(
                doc,
                &todaysmarket::YYInflationCapFloorVolatilities,
                &yyInflationCapFloorVolatilitiesType::YYInflationCapFloorVolatility,
                c,
                rows,
                [](const todays_market_entry& r) {
                    yyInflationCapFloorVolatilitiesType_YYInflationCapFloorVolatility_t e;
                    e.name = required_key(r);
                    static_cast<xsd::string&>(e) = r.target;
                    return e;
                });
        } else if (c.collection == "ZeroInflationCapFloorVolatilities") {
            build_collection(
                doc,
                &todaysmarket::ZeroInflationCapFloorVolatilities,
                &zeroInflationCapFloorVolatilitiesType::ZeroInflationCapFloorVolatility,
                c,
                rows,
                [](const todays_market_entry& r) {
                    zeroInflationCapFloorVolatilitiesType_ZeroInflationCapFloorVolatility_t e;
                    e.name = required_key(r);
                    static_cast<xsd::string&>(e) = r.target;
                    return e;
                });
        } else if (c.collection == "EquityCurves") {
            build_collection(doc,
                             &todaysmarket::EquityCurves,
                             &equityCurvesType::EquityCurve,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 equityCurvesType_EquityCurve_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "EquityVolatilities") {
            build_collection(doc,
                             &todaysmarket::EquityVolatilities,
                             &equityVolatilitiesType::EquityVolatility,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 equityVolatilitiesType_EquityVolatility_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "Securities") {
            build_collection(doc,
                             &todaysmarket::Securities,
                             &securitiesType::Security,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 securitiesType_Security_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "BaseCorrelations") {
            build_collection(doc,
                             &todaysmarket::BaseCorrelations,
                             &baseCorrelationsType::BaseCorrelation,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 baseCorrelationsType_BaseCorrelation_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "CommodityCurves") {
            build_collection(doc,
                             &todaysmarket::CommodityCurves,
                             &commodityCurvesType::CommodityCurve,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 commodityCurvesType_CommodityCurve_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "CommodityVolatilities") {
            build_collection(doc,
                             &todaysmarket::CommodityVolatilities,
                             &commodityVolatilitiesType::CommodityVolatility,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 commodityVolatilitiesType_CommodityVolatility_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "Correlations") {
            build_collection(doc,
                             &todaysmarket::Correlations,
                             &correlationsType::Correlation,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 correlationsType_Correlation_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "BondFutureVolatilities") {
            build_collection(doc,
                             &todaysmarket::BondFutureVolatilities,
                             &bondFutureVolatilitiesType::BondFutureVolatility,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 bondFutureVolatilitiesType_BondFutureVolatility_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else if (c.collection == "IntradayPowerPriceCurves") {
            build_collection(doc,
                             &todaysmarket::IntradayPowerPriceCurves,
                             &intradayPowerPriceCurvesType::IntradayPowerPriceCurve,
                             c,
                             rows,
                             [](const todays_market_entry& r) {
                                 intradayPowerPriceCurvesType_IntradayPowerPriceCurve_t e;
                                 e.name = required_key(r);
                                 static_cast<xsd::string&>(e) = r.target;
                                 return e;
                             });
        } else {
            throw std::runtime_error("todays_market_mapper: unknown collection '" + c.collection +
                                     "'");
        }
    }

    auto configurations = v.configurations;
    std::sort(configurations.begin(), configurations.end(), [](const auto& a, const auto& b) {
        return a.position < b.position;
    });

    for (const auto& c : configurations) {
        configurationType el;
        el.id = c.configuration_id;

        std::vector<const todays_market_configuration_binding*> bindings;
        for (const auto& b : v.bindings) {
            if (b.todays_market_configuration_id == c.id)
                bindings.push_back(&b);
        }
        std::sort(bindings.begin(), bindings.end(), [](const auto* a, const auto* b) {
            return a->position < b->position;
        });

        for (const auto* b : bindings) {
            const auto& name = b->collection;
            if (name == "YieldCurves")
                el.YieldCurvesId = configurationType_YieldCurvesId_t(b->reference);
            else if (name == "DiscountingCurves")
                el.DiscountingCurvesId = configurationType_DiscountingCurvesId_t(b->reference);
            else if (name == "IndexForwardingCurves")
                el.IndexForwardingCurvesId =
                    configurationType_IndexForwardingCurvesId_t(b->reference);
            else if (name == "SwapIndexCurves")
                el.SwapIndexCurvesId = configurationType_SwapIndexCurvesId_t(b->reference);
            else if (name == "ZeroInflationIndexCurves")
                el.ZeroInflationIndexCurvesId =
                    configurationType_ZeroInflationIndexCurvesId_t(b->reference);
            else if (name == "ZeroInflationCapFloorVolatilities")
                el.ZeroInflationCapFloorVolatilitiesId =
                    configurationType_ZeroInflationCapFloorVolatilitiesId_t(b->reference);
            else if (name == "YYInflationIndexCurves")
                el.YYInflationIndexCurvesId =
                    configurationType_YYInflationIndexCurvesId_t(b->reference);
            else if (name == "FxSpots")
                el.FxSpotsId = configurationType_FxSpotsId_t(b->reference);
            else if (name == "BaseCorrelations")
                el.BaseCorrelationsId = configurationType_BaseCorrelationsId_t(b->reference);
            else if (name == "FxVolatilities")
                el.FxVolatilitiesId = configurationType_FxVolatilitiesId_t(b->reference);
            else if (name == "SwaptionVolatilities")
                el.SwaptionVolatilitiesId =
                    configurationType_SwaptionVolatilitiesId_t(b->reference);
            else if (name == "YieldVolatilities")
                el.YieldVolatilitiesId = configurationType_YieldVolatilitiesId_t(b->reference);
            else if (name == "CapFloorVolatilities")
                el.CapFloorVolatilitiesId =
                    configurationType_CapFloorVolatilitiesId_t(b->reference);
            else if (name == "CDSVolatilities")
                el.CDSVolatilitiesId = configurationType_CDSVolatilitiesId_t(b->reference);
            else if (name == "DefaultCurves")
                el.DefaultCurvesId = configurationType_DefaultCurvesId_t(b->reference);
            else if (name == "YYInflationCapFloorVolatilities")
                el.YYInflationCapFloorVolatilitiesId =
                    configurationType_YYInflationCapFloorVolatilitiesId_t(b->reference);
            else if (name == "EquityCurves")
                el.EquityCurvesId = configurationType_EquityCurvesId_t(b->reference);
            else if (name == "EquityVolatilities")
                el.EquityVolatilitiesId = configurationType_EquityVolatilitiesId_t(b->reference);
            else if (name == "Securities")
                el.SecuritiesId = configurationType_SecuritiesId_t(b->reference);
            else if (name == "CommodityCurves")
                el.CommodityCurvesId = configurationType_CommodityCurvesId_t(b->reference);
            else if (name == "CommodityVolatilities")
                el.CommodityVolatilitiesId =
                    configurationType_CommodityVolatilitiesId_t(b->reference);
            else if (name == "Correlations")
                el.CorrelationsId = configurationType_CorrelationsId_t(b->reference);
            else if (name == "BondFutureVolatilities")
                el.BondFutureVolatilitiesId =
                    configurationType_BondFutureVolatilitiesId_t(b->reference);
            else if (name == "IntradayPowerPriceCurves")
                el.IntradayPowerPriceCurvesId =
                    configurationType_IntradayPowerPriceCurvesId_t(b->reference);
            else
                throw std::runtime_error("todays_market_mapper: configuration " +
                                         c.configuration_id + " binds unknown collection '" + name +
                                         "'");
        }

        doc.Configuration.push_back(std::move(el));
    }

    return doc;
}

}
