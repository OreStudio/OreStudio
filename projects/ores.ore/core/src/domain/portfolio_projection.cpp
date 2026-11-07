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
#include <boost/container_hash/hash.hpp>
#include <algorithm>
#include <format>
#include <span>
#include <stdexcept>
#include <unordered_map>

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

/**
 * @brief One list of trades a parent states inline: the element, the field
 * that states each one's type, the structure role it is joined in, and
 * whether each sits in a Derivative wrapper that carries its own Id.
 */
struct embedded_list {
    std::string_view element;
    std::string_view type_field;
    std::string_view role;
    bool wrapped;
};

constexpr embedded_list component_lists[] = {
    {"Trade", "TradeType", "trade", false},
    {"SubTrade", "SubTradeType", "sub_trade", false},
};

constexpr embedded_list underlying_lists[] = {
    {"Trade", "TradeType", "trade", false},
    {"SubTrade", "SubTradeType", "sub_trade", false},
    {"Trade", "TradeType", "derivative_trade", true},
    {"SubTrade", "SubTradeType", "derivative_sub_trade", true},
};

/**
 * @brief A trade type that states other trades inline, and where.
 *
 * A composite trade states its components, and a total return swap or a
 * contract for difference its underlyings. Each embedded trade is booked as
 * a trade of its own and joined to its parent by a structure of the named
 * template, in which the parent holds the parent role.
 */
struct embedding {
    std::string_view parent_type;
    std::string_view data_element;
    std::string_view holder_element;
    std::string_view template_code;
    std::string_view parent_role;
    std::span<const embedded_list> lists;
};

constexpr embedding embeddings[] = {
    {"CompositeTrade",
     "CompositeTradeData",
     "Components",
     "CompositeTrade",
     "composite",
     component_lists},
    {"TotalReturnSwap",
     "TotalReturnSwapData",
     "UnderlyingData",
     "TotalReturnSwap",
     "total_return_swap",
     underlying_lists},
    {"ContractForDifference",
     "ContractForDifferenceData",
     "UnderlyingData",
     "TotalReturnSwap",
     "total_return_swap",
     underlying_lists},
};

auto embedding_of(std::string_view trade_type) -> const embedding* {
    for (const auto& e : embeddings)
        if (e.parent_type == trade_type)
            return &e;
    return nullptr;
}

auto is_parent_role(std::string_view role) -> bool {
    return std::ranges::any_of(embeddings, [&](const auto& e) { return e.parent_role == role; });
}

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
 * @brief Maps each trade a parent states inline as a trade of its own.
 *
 * A list's place is the ordinal within its list, which is the structure
 * role's ordinal. The Derivative wrappers are one list whose entries hold a
 * Trade or a SubTrade, so their ordinal is the wrapper's place, and the
 * wrapper's Id is the embedded trade's ORE id when the trade states none.
 */
void map_embedded(const xml::record_reader& trade,
                  const embedding& e,
                  const std::string& parent_ore_id,
                  std::string_view origin,
                  std::vector<xml::trade_import_item>& out) {
    const auto data = trade.child(e.data_element);
    if (!data)
        return;
    const auto holder = data->child(e.holder_element);
    if (!holder)
        return;
    const auto add = [&](const xml::record_reader& embedded,
                         const embedded_list& list,
                         int sequence_number,
                         std::optional<std::string> wrapper_id) {
        auto item = map_trade(embedded, list.type_field, origin);
        item.membership =
            xml::trade_membership{parent_ore_id, std::string(list.role), sequence_number};
        // A Derivative's Id names the underlying within the swap. The trade
        // keeps an id of its own when it states one, and is booked under the
        // Derivative's Id when it does not.
        if (wrapper_id && !wrapper_id->empty()) {
            item.membership->label = *wrapper_id;
            if (item.ore_id.empty())
                item.ore_id = *wrapper_id;
        }
        if (item.ore_id.empty())
            item.ore_id = derived_component_id(parent_ore_id, list.role, sequence_number);
        out.push_back(std::move(item));
    };
    for (const auto& list : e.lists) {
        if (list.wrapped)
            continue;
        int sequence_number = 0;
        for (const auto& embedded : holder->children(list.element))
            add(embedded, list, ++sequence_number, std::nullopt);
    }
    if (!std::ranges::any_of(e.lists, &embedded_list::wrapped))
        return;
    int sequence_number = 0;
    for (const auto& wrapper : holder->children("Derivative")) {
        ++sequence_number;
        for (const auto& list : e.lists)
            if (list.wrapped)
                if (const auto embedded = wrapper.child(list.element))
                    add(*embedded, list, sequence_number, wrapper.text("Id"));
    }
}

}

std::optional<parent_structure> portfolio_projection::structure_of(std::string_view trade_type) {
    const auto* e = embedding_of(trade_type);
    if (e == nullptr)
        return std::nullopt;
    return parent_structure{e->template_code, e->parent_role};
}

std::vector<xml::trade_import_item> portfolio_projection::map(const document& v,
                                                              std::string_view origin) {
    const auto& type = *pf::portfolio_document().root;
    const auto root = xml::record_reader(type, &v);

    std::vector<xml::trade_import_item> r;
    for (const auto& trade : root.children("Trade")) {
        auto item = map_trade(trade, "TradeType", origin);
        const auto* e = embedding_of(item.anchor.trade_type);
        const auto parent_ore_id = item.ore_id;
        r.push_back(std::move(item));
        if (e != nullptr)
            map_embedded(trade, *e, parent_ore_id, origin, r);
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
    std::unordered_map<trade_key,
                       const trading::messaging::trade_export_item*,
                       boost::hash<trade_key>>
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

    const auto members_in_role = [&](const boost::uuids::uuid& structure_id,
                                     std::string_view role) {
        std::vector<const trading::domain::structure_member*> members;
        for (const auto& member : instruments.structure_members)
            if (member.structure_id == structure_id && member.role == role)
                members.push_back(&member);
        return members;
    };

    const auto write_embedded_trade = [&](xml::record_writer& writer,
                                          const embedded_list& list,
                                          const trading::messaging::trade_export_item& child,
                                          bool states_id) {
        if (states_id)
            writer.set_text("id", child.ore_id);
        writer.set_spelling(list.type_field, child.anchor.trade_type, "trade type");
        if (child.envelope)
            write_envelope(writer, *child.envelope);
        write_instrument(writer, child);
    };

    // Writes the trades a parent states inline, from the structure that joins
    // them to it, in the lists and the order the document stated them.
    const auto write_embedded = [&](xml::record_writer& trade,
                                    const trading::messaging::trade_export_item& parent,
                                    const embedding& e) {
        const auto self = member_of.find(parent.anchor.id);
        if (self == member_of.end())
            return;
        const auto structure_id = self->second->structure_id;
        // A parent whose own terms were not kept writes no data element, and
        // the schema requires more of one than the embedded trades fill.
        auto data = trade.chosen_child(e.data_element);
        if (!data)
            return;
        std::optional<xml::record_writer> holder;
        const auto holder_of = [&]() -> xml::record_writer& {
            if (!holder)
                holder = data->child(e.holder_element);
            return *holder;
        };
        const auto child_of = [&](const trading::domain::structure_member* member)
            -> const trading::messaging::trade_export_item* {
            const auto found = item_of.find(member->trade_id);
            return found == item_of.end() ? nullptr : found->second;
        };
        const auto derived = [&](const embedded_list& list, int sequence_number) {
            return derived_component_id(parent.ore_id, list.role, sequence_number);
        };

        for (const auto& list : e.lists) {
            if (list.wrapped)
                continue;
            auto members = members_in_role(structure_id, list.role);
            std::ranges::sort(members, {}, &trading::domain::structure_member::sequence_number);
            for (const auto* member : members) {
                const auto* child = child_of(member);
                if (child == nullptr)
                    continue;
                auto writer = holder_of().append(list.element);
                const bool states_id = !child->ore_id.empty() &&
                                       child->ore_id != derived(list, member->sequence_number);
                write_embedded_trade(writer, list, *child, states_id);
            }
        }

        // The Derivative wrappers are one list whatever they hold, so their
        // members are merged and ordered by the wrapper's place.
        std::vector<std::pair<const trading::domain::structure_member*, const embedded_list*>>
            wrapped;
        for (const auto& list : e.lists)
            if (list.wrapped)
                for (const auto* member : members_in_role(structure_id, list.role))
                    wrapped.emplace_back(member, &list);
        std::ranges::sort(
            wrapped, {}, [](const auto& entry) { return entry.first->sequence_number; });
        for (const auto& [member, list] : wrapped) {
            const auto* child = child_of(member);
            if (child == nullptr)
                continue;
            auto wrapper = holder_of().append("Derivative");
            if (member->member_label) {
                wrapper.set_text("Id", *member->member_label);
            } else if (!child->ore_id.empty() &&
                       child->ore_id != derived(*list, member->sequence_number)) {
                wrapper.set_text("Id", child->ore_id);
            }
            auto writer = wrapper.child(list->element);
            const bool states_own_id = member->member_label && !child->ore_id.empty() &&
                                       child->ore_id != *member->member_label;
            write_embedded_trade(writer, *list, *child, states_own_id);
        }
    };

    for (const auto& item : items) {
        const auto role = role_of(item.anchor.id);
        if (!role.empty() && !is_parent_role(role))
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
        if (const auto* e = embedding_of(item.anchor.trade_type); e != nullptr && !role.empty())
            write_embedded(trade, item, *e);
    }

    root.state("Trade");
    return doc;
}

}
