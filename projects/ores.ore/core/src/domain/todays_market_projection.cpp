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
#include "ores.ore.core/domain/todays_market_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/projection.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <format>
#include <functional>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

namespace xg = xml::generated::todays_market;

using analytics::domain::todays_market_collection;
using analytics::domain::todays_market_configuration;
using analytics::domain::todays_market_configuration_binding;
using analytics::domain::todays_market_entry;
using xml::record_type;
using xml::projection::child;
using xml::projection::state;

using entities = analytics::messaging::todays_market_document;

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

template <typename Row>
bool by_position(const Row* lhs, const Row* rhs) {
    return lhs->position < rhs->position;
}

// A string attribute, or nothing when the document left it out. An attribute
// written empty is present, and the entities keep the difference.
template <typename Type>
std::optional<std::string> attribute_of(const record_type& type,
                                        const Type& record,
                                        const std::string_view name,
                                        const std::string& value) {
    if (!xml::is_present(type, &record, name))
        return std::nullopt;
    return value;
}

// What one collection needs while the document is read: the entities being
// filled, the next collection position, and the configuration they belong to.
struct reading {
    entities& out;
    const boost::uuids::uuid config_id;
    int collection_position = 0;
};

// One collection of the record and the entity rows it becomes. Reading appends
// the collection rows and their entries; writing rebuilds one element from a
// collection row and its entries.
struct collection_codec {
    std::string_view name;
    std::function<void(reading&, const xg::todaysmarket&)> read;
    std::function<void(xg::todaysmarket&,
                       const todays_market_collection&,
                       const std::vector<const todays_market_entry*>&)>
        write;
};

template <typename Wrapper, typename Entry, typename ReadEntry, typename WriteEntry>
collection_codec codec(std::string_view name,
                       std::string_view entry_name,
                       std::vector<Wrapper> xg::todaysmarket::*member,
                       std::vector<Entry> Wrapper::*entries,
                       ReadEntry read_entry,
                       WriteEntry write_entry) {
    const auto read = [=](reading& in, const xg::todaysmarket& doc) {
        const auto& root = *xg::todays_market_document().root;
        const auto& wrapper_type = child(root, name);
        const auto& entry_type = child(wrapper_type, entry_name);
        for (const auto& w : doc.*member) {
            todays_market_collection c;
            c.id = new_uuid();
            c.todays_market_config_id = in.config_id;
            c.collection = std::string(name);
            c.collection_id = attribute_of(wrapper_type, w, "id", w.id);
            c.position = in.collection_position++;
            set_audit(c);
            in.out.collections.push_back(c);

            int position = 0;
            for (const auto& e : w.*entries) {
                todays_market_entry row;
                row.id = new_uuid();
                row.todays_market_config_id = in.config_id;
                row.todays_market_collection_id = c.id;
                read_entry(entry_type, e, row);
                row.position = position++;
                set_audit(row);
                in.out.entries.push_back(std::move(row));
            }
        }
    };

    const auto write = [=](xg::todaysmarket& doc,
                           const todays_market_collection& c,
                           const std::vector<const todays_market_entry*>& rows) {
        const auto& root = *xg::todays_market_document().root;
        const auto& wrapper_type = child(root, name);
        const auto& entry_type = child(wrapper_type, entry_name);
        Wrapper w;
        if (c.collection_id) {
            w.id = *c.collection_id;
            state(wrapper_type, &w, "id");
        }
        for (const auto* r : rows) {
            Entry e;
            write_entry(entry_type, *r, e);
            (w.*entries).push_back(std::move(e));
        }
        if (!rows.empty())
            state(wrapper_type, &w, entry_name);
        (doc.*member).push_back(std::move(w));
        state(root, &doc, name);
    };

    return {name, read, write};
}

// The reference text of an entry is the text of its element. Every entry
// except SwapIndex has one.
template <typename Entry>
void read_text(const Entry& e, todays_market_entry& row) {
    row.target = e.value;
}

template <typename Entry>
void write_text(const record_type& type, const todays_market_entry& row, Entry& e) {
    e.value = row.target;
    xml::mark_text_present(type, &e);
}

// The key is absent only where the schema makes it optional. Anywhere else a
// row without one cannot be written back as a valid document.
const std::string& required_key(const todays_market_entry& r) {
    if (!r.key_value)
        throw std::runtime_error("todays_market_projection: entry " + r.target +
                                 " has no key value, which its collection requires");
    return *r.key_value;
}

// A collection whose entry is a reference text keyed by one required
// attribute: =name=, =currency= or =pair=.
template <typename Wrapper, typename Entry>
collection_codec keyed(std::string_view name,
                       std::string_view entry_name,
                       std::vector<Wrapper> xg::todaysmarket::*member,
                       std::vector<Entry> Wrapper::*entries,
                       std::string_view key_name,
                       std::string Entry::*key) {
    return codec(
        name,
        entry_name,
        member,
        entries,
        [=](const record_type& type, const Entry& e, todays_market_entry& row) {
            row.key_value = attribute_of(type, e, key_name, e.*key);
            read_text(e, row);
        },
        [=](const record_type& type, const todays_market_entry& row, Entry& e) {
            e.*key = required_key(row);
            state(type, &e, key_name);
            write_text(type, row, e);
        });
}

// The two collections whose entry carries an optional =key= and an optional
// =currency=, either of which the document may leave out.
template <typename Wrapper, typename Entry>
collection_codec optionally_keyed(std::string_view name,
                                  std::string_view entry_name,
                                  std::vector<Wrapper> xg::todaysmarket::*member,
                                  std::vector<Entry> Wrapper::*entries) {
    return codec(
        name,
        entry_name,
        member,
        entries,
        [](const record_type& type, const Entry& e, todays_market_entry& row) {
            row.key_value = attribute_of(type, e, "key", e.key);
            row.key_value_2 = attribute_of(type, e, "currency", e.currency);
            read_text(e, row);
        },
        [](const record_type& type, const todays_market_entry& row, Entry& e) {
            if (row.key_value) {
                e.key = *row.key_value;
                state(type, &e, "key");
            }
            if (row.key_value_2) {
                e.currency = *row.key_value_2;
                state(type, &e, "currency");
            }
            write_text(type, row, e);
        });
}

// SwapIndexCurves is the exception: its entry has no reference text, and its
// value is the nested Discounting child.
collection_codec swap_index_curves() {
    return codec(
        "SwapIndexCurves",
        "SwapIndex",
        &xg::todaysmarket::swap_index_curves,
        &xg::swap_index_curves_type::swap_index,
        [](const record_type& type, const xg::swap_index& e, todays_market_entry& row) {
            row.key_value = attribute_of(type, e, "name", e.name);
            row.target = "";
            row.discounting = e.discounting;
        },
        [](const record_type& type, const todays_market_entry& row, xg::swap_index& e) {
            e.name = required_key(row);
            state(type, &e, "name");
            e.discounting = row.discounting.value_or("");
            state(type, &e, "Discounting");
        });
}

// The collections in the order they are read, which is the order their
// positions are numbered in.
const std::vector<collection_codec>& collections() {
    using doc = xg::todaysmarket;
    static const std::vector<collection_codec> all = [] {
        std::vector<collection_codec> c;
        c.push_back(keyed("YieldCurves",
                          "YieldCurve",
                          &doc::yield_curves,
                          &xg::yield_curves_type::yield_curve,
                          "name",
                          &xg::yield_curve::name));
        c.push_back(keyed("IndexForwardingCurves",
                          "Index",
                          &doc::index_forwarding_curves,
                          &xg::index_forwarding_curves_type::index,
                          "name",
                          &xg::index::name));
        c.push_back(keyed("ZeroInflationIndexCurves",
                          "ZeroInflationIndexCurve",
                          &doc::zero_inflation_index_curves,
                          &xg::zero_inflation_index_curves_type::zero_inflation_index_curve,
                          "name",
                          &xg::zero_inflation_index_curve::name));
        c.push_back(keyed("YYInflationIndexCurves",
                          "YYInflationIndexCurve",
                          &doc::yy_inflation_index_curves,
                          &xg::yy_inflation_index_curves_type::yy_inflation_index_curve,
                          "name",
                          &xg::yy_inflation_index_curve::name));
        c.push_back(keyed("YieldVolatilities",
                          "YieldVolatility",
                          &doc::yield_volatilities,
                          &xg::yield_volatilities_type::yield_volatility,
                          "name",
                          &xg::yield_volatility::name));
        c.push_back(keyed("CDSVolatilities",
                          "CDSVolatility",
                          &doc::cds_volatilities,
                          &xg::cds_volatilities_type::cds_volatility,
                          "name",
                          &xg::cds_volatility::name));
        c.push_back(keyed("DefaultCurves",
                          "DefaultCurve",
                          &doc::default_curves,
                          &xg::default_curves_type::default_curve,
                          "name",
                          &xg::default_curve::name));
        c.push_back(keyed("EquityCurves",
                          "EquityCurve",
                          &doc::equity_curves,
                          &xg::equity_curves_type::equity_curve,
                          "name",
                          &xg::equity_curve::name));
        c.push_back(keyed("EquityVolatilities",
                          "EquityVolatility",
                          &doc::equity_volatilities,
                          &xg::equity_volatilities_type::equity_volatility,
                          "name",
                          &xg::equity_volatility::name));
        c.push_back(keyed("Securities",
                          "Security",
                          &doc::securities,
                          &xg::securities_type::security,
                          "name",
                          &xg::security::name));
        c.push_back(keyed("BaseCorrelations",
                          "BaseCorrelation",
                          &doc::base_correlations,
                          &xg::base_correlations_type::base_correlation,
                          "name",
                          &xg::base_correlation::name));
        c.push_back(keyed("CommodityCurves",
                          "CommodityCurve",
                          &doc::commodity_curves,
                          &xg::commodity_curves_type::commodity_curve,
                          "name",
                          &xg::commodity_curve::name));
        c.push_back(keyed("CommodityVolatilities",
                          "CommodityVolatility",
                          &doc::commodity_volatilities,
                          &xg::commodity_volatilities_type::commodity_volatility,
                          "name",
                          &xg::commodity_volatility::name));
        c.push_back(keyed("Correlations",
                          "Correlation",
                          &doc::correlations,
                          &xg::correlations_type::correlation,
                          "name",
                          &xg::correlation::name));
        c.push_back(keyed("BondFutureVolatilities",
                          "BondFutureVolatility",
                          &doc::bond_future_volatilities,
                          &xg::bond_future_volatilities_type::bond_future_volatility,
                          "name",
                          &xg::bond_future_volatility::name));
        c.push_back(keyed("IntradayPowerPriceCurves",
                          "IntradayPowerPriceCurve",
                          &doc::intraday_power_price_curves,
                          &xg::intraday_power_price_curves_type::intraday_power_price_curve,
                          "name",
                          &xg::intraday_power_price_curve::name));
        c.push_back(keyed("DiscountingCurves",
                          "DiscountingCurve",
                          &doc::discounting_curves,
                          &xg::discount_curves_type::discounting_curve,
                          "currency",
                          &xg::discounting_curve::currency));
        c.push_back(keyed("FxSpots",
                          "FxSpot",
                          &doc::fx_spots,
                          &xg::fx_spots_type::fx_spot,
                          "pair",
                          &xg::fx_spot::pair));
        c.push_back(keyed("FxVolatilities",
                          "FxVolatility",
                          &doc::fx_volatilities,
                          &xg::fx_volatilities_type::fx_volatility,
                          "pair",
                          &xg::fx_volatility::pair));
        c.push_back(optionally_keyed("SwaptionVolatilities",
                                     "SwaptionVolatility",
                                     &doc::swaption_volatilities,
                                     &xg::swaption_volatilities_type::swaption_volatility));
        c.push_back(optionally_keyed("CapFloorVolatilities",
                                     "CapFloorVolatility",
                                     &doc::cap_floor_volatilities,
                                     &xg::cap_floor_volatilities_type::cap_floor_volatility));
        c.push_back(keyed(
            "ZeroInflationCapFloorVolatilities",
            "ZeroInflationCapFloorVolatility",
            &doc::zero_inflation_cap_floor_volatilities,
            &xg::zero_inflation_cap_floor_volatilities_type::zero_inflation_cap_floor_volatility,
            "name",
            &xg::zero_inflation_cap_floor_volatility::name));
        c.push_back(
            keyed("YYInflationCapFloorVolatilities",
                  "YYInflationCapFloorVolatility",
                  &doc::yy_inflation_cap_floor_volatilities,
                  &xg::yy_inflation_cap_floor_volatilities_type::yy_inflation_cap_floor_volatility,
                  "name",
                  &xg::yy_inflation_cap_floor_volatility::name));
        c.push_back(swap_index_curves());
        return c;
    }();
    return all;
}

// The attribute of a configuration that names the collection it binds, for
// each of the collections. The order is the order bindings are numbered in.
struct binding_attribute {
    std::string_view collection;
    std::string_view attribute;
    std::string xg::configuration_type::*member;
};

const std::vector<binding_attribute>& binding_attributes() {
    using c = xg::configuration_type;
    static const std::vector<binding_attribute> all = {
        {"YieldCurves", "YieldCurvesId", &c::yield_curves_id},
        {"DiscountingCurves", "DiscountingCurvesId", &c::discounting_curves_id},
        {"IndexForwardingCurves", "IndexForwardingCurvesId", &c::index_forwarding_curves_id},
        {"SwapIndexCurves", "SwapIndexCurvesId", &c::swap_index_curves_id},
        {"ZeroInflationIndexCurves",
         "ZeroInflationIndexCurvesId",
         &c::zero_inflation_index_curves_id},
        {"ZeroInflationCapFloorVolatilities",
         "ZeroInflationCapFloorVolatilitiesId",
         &c::zero_inflation_cap_floor_volatilities_id},
        {"YYInflationIndexCurves", "YYInflationIndexCurvesId", &c::yy_inflation_index_curves_id},
        {"FxSpots", "FxSpotsId", &c::fx_spots_id},
        {"BaseCorrelations", "BaseCorrelationsId", &c::base_correlations_id},
        {"FxVolatilities", "FxVolatilitiesId", &c::fx_volatilities_id},
        {"SwaptionVolatilities", "SwaptionVolatilitiesId", &c::swaption_volatilities_id},
        {"YieldVolatilities", "YieldVolatilitiesId", &c::yield_volatilities_id},
        {"CapFloorVolatilities", "CapFloorVolatilitiesId", &c::cap_floor_volatilities_id},
        {"CDSVolatilities", "CDSVolatilitiesId", &c::cds_volatilities_id},
        {"DefaultCurves", "DefaultCurvesId", &c::default_curves_id},
        {"YYInflationCapFloorVolatilities",
         "YYInflationCapFloorVolatilitiesId",
         &c::yy_inflation_cap_floor_volatilities_id},
        {"EquityCurves", "EquityCurvesId", &c::equity_curves_id},
        {"EquityVolatilities", "EquityVolatilitiesId", &c::equity_volatilities_id},
        {"Securities", "SecuritiesId", &c::securities_id},
        {"CommodityCurves", "CommodityCurvesId", &c::commodity_curves_id},
        {"CommodityVolatilities", "CommodityVolatilitiesId", &c::commodity_volatilities_id},
        {"Correlations", "CorrelationsId", &c::correlations_id},
        {"BondFutureVolatilities", "BondFutureVolatilitiesId", &c::bond_future_volatilities_id},
        {"IntradayPowerPriceCurves",
         "IntradayPowerPriceCurvesId",
         &c::intraday_power_price_curves_id},
    };
    return all;
}

}

ores::analytics::messaging::todays_market_document
todays_market_projection::map(const document& v) {
    entities mapped;

    auto& config = mapped.config;
    config.id = new_uuid();
    config.name = "TodaysMarket";
    config.description = "Imported from ORE XML";
    config.config_variant = "";
    set_audit(config);

    const auto& configuration_type = child(*xg::todays_market_document().root, "Configuration");
    int configuration_position = 0;
    for (const auto& c : v.configuration) {
        todays_market_configuration row;
        row.id = new_uuid();
        row.todays_market_config_id = config.id;
        row.configuration_id = c.id;
        row.position = configuration_position++;
        set_audit(row);
        mapped.configurations.push_back(row);

        int binding_position = 0;
        for (const auto& binding : binding_attributes()) {
            if (!xml::is_present(configuration_type, &c, binding.attribute))
                continue;
            todays_market_configuration_binding b;
            b.id = new_uuid();
            b.todays_market_configuration_id = row.id;
            b.collection = std::string(binding.collection);
            b.reference = c.*binding.member;
            b.position = binding_position++;
            set_audit(b);
            mapped.bindings.push_back(b);
        }
    }

    reading in{mapped, config.id};
    for (const auto& collection : collections())
        collection.read(in, v);
    return mapped;
}

todays_market_projection::document
todays_market_projection::reverse(const ores::analytics::messaging::todays_market_document& v) {
    const auto* root = xg::todays_market_document().root;
    document doc;

    std::vector<const todays_market_collection*> ordered;
    for (const auto& c : v.collections)
        ordered.push_back(&c);
    std::stable_sort(ordered.begin(), ordered.end(), by_position<todays_market_collection>);

    for (const auto* c : ordered) {
        const auto found = std::ranges::find_if(
            collections(), [&](const collection_codec& k) { return k.name == c->collection; });
        if (found == collections().end())
            throw std::runtime_error("todays_market_projection: unknown collection '" +
                                     c->collection + "'");

        std::vector<const todays_market_entry*> rows;
        for (const auto& r : v.entries) {
            if (r.todays_market_collection_id == c->id)
                rows.push_back(&r);
        }
        std::stable_sort(rows.begin(), rows.end(), by_position<todays_market_entry>);
        found->write(doc, *c, rows);
    }

    std::vector<const todays_market_configuration*> configurations;
    for (const auto& c : v.configurations)
        configurations.push_back(&c);
    std::stable_sort(
        configurations.begin(), configurations.end(), by_position<todays_market_configuration>);

    const auto& configuration_type = child(*root, "Configuration");
    for (const auto* c : configurations) {
        xg::configuration_type el;
        el.id = c->configuration_id;
        state(configuration_type, &el, "id");

        std::vector<const todays_market_configuration_binding*> bindings;
        for (const auto& b : v.bindings) {
            if (b.todays_market_configuration_id == c->id)
                bindings.push_back(&b);
        }
        std::stable_sort(
            bindings.begin(), bindings.end(), by_position<todays_market_configuration_binding>);
        for (const auto* b : bindings) {
            const auto found =
                std::ranges::find_if(binding_attributes(), [&](const binding_attribute& a) {
                    return a.collection == b->collection;
                });
            if (found == binding_attributes().end())
                throw std::runtime_error("todays_market_projection: unknown binding '" +
                                         b->collection + "'");
            el.*found->member = b->reference;
            state(configuration_type, &el, found->attribute);
        }
        doc.configuration.push_back(std::move(el));
        state(*root, &doc, "Configuration");
    }
    return doc;
}

todays_market_projection::document todays_market_projection::read(std::string_view text,
                                                                  std::string_view origin) {
    document record;
    const auto bound = xml::binder::bind(xg::todays_market_document(), text, &record);
    if (!bound) {
        const auto where = origin.empty() ? std::string("today's market") : std::string(origin);
        throw std::invalid_argument(
            std::format("{}: line {}: {}", where, bound.error().line, bound.error().message));
    }
    return record;
}

std::string todays_market_projection::write(const document& v) {
    std::string out;
    const auto emitted = xml::emitter::emit(xg::todays_market_document(), &v, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

}
