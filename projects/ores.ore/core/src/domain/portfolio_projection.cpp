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
#include "ores.ore.core/domain/portfolio_projection.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include <algorithm>
#include <format>
#include <stdexcept>
#include <unordered_map>
#include <boost/container_hash/hash.hpp>

namespace ores::ore::domain {

namespace {

inline auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.ore.domain.portfolio_projection");
    return instance;
}

namespace pf = xml::generated::portfolio;

using trading::domain::trade_envelope_data;
using trading::domain::trade_envelope_field;

/**
 * @brief Whether the table states the spelling for an enumerated member.
 *
 * A trade whose type ORE does not name has no document to write. The caller
 * states the map its own way, so the projection checks the spelling before it
 * appends a trade rather than writing a type no reader accepts.
 */
auto portfolio_projection_accepts_spelling(const xml::record_type& type,
                                           std::string_view name,
                                           std::string_view value) -> bool {
    const auto* f = xml::find_field(type, name, false);
    if (f == nullptr || f->tokens == nullptr)
        return false;
    for (std::int16_t i = 0; f->tokens[i] != nullptr; ++i)
        if (value == f->tokens[i])
            return true;
    return false;
}

auto read_envelope(xml::record_reader trade) -> std::optional<trade_envelope_data> {
    const auto envelope = trade.child("Envelope");
    if (!envelope)
        return std::nullopt;

    trade_envelope_data r;
    if (const auto counter_party = envelope->text("CounterParty"))
        r.counter_party = *counter_party;
    if (const auto netting_set = envelope->text("NettingSetId"))
        r.netting_set_id = *netting_set;

    if (const auto ids = envelope->child("PortfolioIds")) {
        const auto values = ids->values("PortfolioId");
        if (!values.empty())
            r.portfolio_ids = values;
    }

    if (const auto fields = envelope->child("AdditionalFields")) {
        std::vector<trade_envelope_field> values;
        for (const auto& extra : fields->residuals())
            values.push_back({extra.name, extra.text});
        if (!values.empty())
            r.additional_fields = std::move(values);
    }
    return r;
}

void write_envelope(xml::record_writer& trade, const trade_envelope_data& v) {
    auto envelope = trade.child("Envelope");
    if (v.counter_party)
        envelope.set_text("CounterParty", *v.counter_party);
    if (v.netting_set_id)
        envelope.set_text("NettingSetId", *v.netting_set_id);

    if (v.portfolio_ids) {
        auto ids = envelope.child("PortfolioIds");
        for (const auto& id : *v.portfolio_ids)
            ids.append_value("PortfolioId", id);
    }

    if (v.additional_fields) {
        auto fields = envelope.child("AdditionalFields");
        for (const auto& field : *v.additional_fields)
            fields.append_residual(field.name, field.value);
    }
}

}

auto instrument_projections() -> const std::vector<instrument_projection>& {
    // The old dispatcher tried the swap mappers before the FX one, the bond
    // one, the credit one, the equity one, the commodity one, the scripted
    // one and the composite one last, so the registry keeps that order.
    static const std::vector<instrument_projection> all = {
        {"rates", &read_rate_instrument, &write_rate_instrument},
        {"fx", &read_fx_instrument, &write_fx_instrument},
        {"bond", &read_bond_instrument, &write_bond_instrument},
        {"credit", &read_credit_instrument, &write_credit_instrument},
        {"equity", &read_equity_instrument, &write_equity_instrument},
        {"commodity", &read_commodity_instrument, &write_commodity_instrument},
        {"scripted", &read_scripted_instrument, &write_scripted_instrument},
        {"composite", &read_composite_instrument, &write_composite_instrument},
    };
    return all;
}

auto portfolio_projection::read(std::string_view text, std::string_view origin) -> document {
    document record;
    const auto bound = xml::binder::bind(pf::portfolio_document(), text, &record);
    if (!bound) {
        const auto where = origin.empty() ? std::string("portfolio") : std::string(origin);
        throw std::invalid_argument(
            std::format("{}: line {}: {}", where, bound.error().line, bound.error().message));
    }
    return record;
}

std::string portfolio_projection::write(const document& v) {
    std::string out;
    const auto emitted = xml::emitter::emit(pf::portfolio_document(), &v, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

written_portfolio portfolio_projection::write_leaving_out(const document& v) {
    written_portfolio result;
    std::string out;
    if (xml::emitter::emit(pf::portfolio_document(), &v, out)) {
        result.xml = std::move(out);
        return result;
    }

    // A trade's product lives in the columns the document shares, so each
    // trade is tried in a copy that keeps the columns and holds that trade
    // alone. The copy is made once and only when the whole document refuses.
    auto alone = v;
    auto kept = v;
    kept.trade.clear();
    const auto& type = *pf::portfolio_document().root;
    for (const auto& trade : v.trade) {
        alone.trade = {trade};
        const auto emitted = xml::emitter::emit(pf::portfolio_document(), &alone, out);
        if (emitted) {
            kept.trade.push_back(trade);
            continue;
        }
        std::string trade_type;
        for (const auto& one : xml::record_reader(type, &alone).children("Trade"))
            trade_type = one.spelling("TradeType").value_or("");
        result.refused.push_back(refused_trade{
            .trade_id = trade.id, .trade_type = trade_type, .message = emitted.error().message});
    }
    if (kept.trade.empty() && !result.refused.empty())
        throw std::runtime_error(
            "No trade of the portfolio can be written; the first refusal is: " +
            result.refused.front().message);
    result.xml = write(kept);
    return result;
}

namespace {

/**
 * @brief Maps one trade the document states, a top level trade or a component.
 *
 * A trade whose product no mapping can hold still imports: the anchor and the
 * envelope are the trade's, and the instrument is the part the mapping failed
 * on. The import reports it rather than losing the trade.
 */
auto map_trade(const xml::record_reader& trade,
               std::string_view type_field,
               std::string_view origin) -> xml::trade_import_item {
    const auto trade_type = trade.spelling(type_field);
    if (!trade_type)
        throw std::runtime_error("a trade the portfolio document states has no type");

    xml::trade_import_item item;
    item.ore_id = trade.text("id").value_or("");
    item.anchor.trade_type = *trade_type;
    item.envelope = read_envelope(trade);
    try {
        for (const auto& family : instrument_projections()) {
            auto instrument = family.read(trade, *trade_type);
            if (!instrument)
                continue;
            item.instrument = std::move(*instrument);
            break;
        }
    } catch (const std::exception& e) {
        const auto where = origin.empty() ? std::string("the document") : std::string(origin);
        BOOST_LOG_SEV(lg(), ores::logging::warn)
            << "Failed to map trade " << item.ore_id << " of type " << item.anchor.trade_type
            << " from " << where << ": " << e.what();
    }
    return item;
}

/// A composite's component lists: the element, the type field and the role.
struct component_list {
    std::string_view element;
    std::string_view type_field;
    std::string_view role;
};

constexpr component_list component_lists[] = {
    {"Trade", "TradeType", "trade"},
    {"SubTrade", "SubTradeType", "sub_trade"},
};

/**
 * @brief The ORE id a component without one of its own is booked under.
 *
 * Every booked trade names the ORE id it came from, and a SubTrade may state
 * none. The id is derived from the composite's and the component's place, so
 * the writer can tell it from one the document stated and leave it out.
 */
auto derived_component_id(std::string_view parent_ore_id,
                          std::string_view role,
                          int sequence_number) -> std::string {
    return std::format("{}/{}/{}", parent_ore_id, role, sequence_number);
}

/**
 * @brief Maps each component a composite trade states as a trade of its own.
 *
 * A component is a Trade or a SubTrade, each a list of its own, and its place
 * is its ordinal within its list, which is the structure role's ordinal.
 */
void map_components(const xml::record_reader& trade,
                    const std::string& parent_ore_id,
                    std::string_view origin,
                    std::vector<xml::trade_import_item>& out) {
    const auto data = trade.child("CompositeTradeData");
    if (!data)
        return;
    const auto components = data->child("Components");
    if (!components)
        return;
    for (const auto& list : component_lists) {
        int sequence_number = 0;
        for (const auto& component : components->children(list.element)) {
            auto item = map_trade(component, list.type_field, origin);
            item.membership =
                xml::trade_membership{parent_ore_id, std::string(list.role), ++sequence_number};
            if (item.ore_id.empty())
                item.ore_id = derived_component_id(parent_ore_id, list.role, sequence_number);
            out.push_back(std::move(item));
        }
    }
}

}

std::vector<xml::trade_import_item> portfolio_projection::map(const document& v,
                                                              std::string_view origin) {
    const auto& type = *pf::portfolio_document().root;
    const auto root = xml::record_reader(type, &v);

    std::vector<xml::trade_import_item> r;
    for (const auto& trade : root.children("Trade")) {
        auto item = map_trade(trade, "TradeType", origin);
        const bool composite = item.anchor.trade_type == "CompositeTrade";
        const auto parent_ore_id = item.ore_id;
        r.push_back(std::move(item));
        if (composite)
            map_components(trade, parent_ore_id, origin, r);
    }
    return r;
}

portfolio_projection::document
portfolio_projection::reverse(const std::vector<trading::messaging::trade_export_item>& items,
                              const trading::domain::instrument_batch& instruments,
                              const portfolio_reference_curves& reference_curves) {
    document doc;
    const auto& type = *pf::portfolio_document().root;
    xml::record_writer root(type, &doc);

    const auto* trade_type = xml::find_child_type(type, "Trade");
    if (trade_type == nullptr)
        throw std::runtime_error("the portfolio table has no Trade element");

    // A component of a composite trade is a trade of its own, joined to the
    // composite by a structure. It is written inside the composite, not at
    // the top level.
    using trade_key = boost::uuids::uuid;
    std::unordered_map<trade_key, const trading::domain::structure_member*, boost::hash<trade_key>>
        member_of;
    for (const auto& member : instruments.structure_members)
        member_of[member.trade_id] = &member;
    std::unordered_map<trade_key, const trading::messaging::trade_export_item*, boost::hash<trade_key>>
        item_of;
    for (const auto& item : items)
        item_of[item.anchor.id] = &item;
    const auto role_of = [&](const trade_key& id) -> std::string_view {
        const auto found = member_of.find(id);
        return found == member_of.end() ? std::string_view{} : found->second->role;
    };

    const auto write_instrument = [&](xml::record_writer& trade,
                                      const trading::messaging::trade_export_item& item) {
        const auto instrument = trading::domain::rebuild_instrument(instruments, item.anchor.id);
        if (std::holds_alternative<std::monostate>(instrument))
            return;
        for (const auto& family : instrument_projections())
            if (family.write(trade, item.anchor.trade_type, instrument, reference_curves))
                break;
    };

    const auto write_components = [&](xml::record_writer& trade,
                                      const trading::messaging::trade_export_item& composite_item) {
        const auto& composite_id = composite_item.anchor.id;
        const auto composite = member_of.find(composite_id);
        if (composite == member_of.end())
            return;
        auto data = trade.chosen_child("CompositeTradeData");
        if (!data)
            return;
        std::optional<xml::record_writer> components;
        for (const auto& list : component_lists) {
            std::vector<const trading::domain::structure_member*> members;
            for (const auto& member : instruments.structure_members)
                if (member.structure_id == composite->second->structure_id &&
                    member.role == list.role)
                    members.push_back(&member);
            std::sort(members.begin(), members.end(), [](const auto* a, const auto* b) {
                return a->sequence_number < b->sequence_number;
            });
            for (const auto* member : members) {
                const auto found = item_of.find(member->trade_id);
                if (found == item_of.end())
                    continue;
                const auto& child = *found->second;
                if (!components)
                    components = data->child("Components");
                auto writer = components->append(list.element);
                const auto derived = derived_component_id(
                    composite_item.ore_id, list.role, member->sequence_number);
                if (!child.ore_id.empty() && child.ore_id != derived)
                    writer.set_text("id", child.ore_id);
                writer.set_spelling(list.type_field, child.anchor.trade_type, "trade type");
                if (child.envelope)
                    write_envelope(writer, *child.envelope);
                write_instrument(writer, child);
            }
        }
    };

    for (const auto& item : items) {
        const auto role = role_of(item.anchor.id);
        if (role == "trade" || role == "sub_trade")
            continue;

        if (!portfolio_projection_accepts_spelling(
                *trade_type, "TradeType", item.anchor.trade_type)) {
            // The schema has no such trade type, so there is no document to
            // write. Naming the trade keeps the loss visible to the operator.
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "Dropping trade " << item.ore_id << " of type " << item.anchor.trade_type
                << ": the schema names no such trade type";
            continue;
        }

        auto trade = root.append("Trade");
        trade.set_spelling("TradeType", item.anchor.trade_type, "trade type");
        if (!item.ore_id.empty())
            trade.set_text("id", item.ore_id);
        if (item.envelope)
            write_envelope(trade, *item.envelope);
        write_instrument(trade, item);
        if (role == "composite")
            write_components(trade, item);
    }

    root.state("Trade");
    return doc;
}

}
