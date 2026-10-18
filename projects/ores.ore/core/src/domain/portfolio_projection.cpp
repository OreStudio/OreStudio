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
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include <format>
#include <stdexcept>

namespace ores::ore::domain {

namespace {

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
auto accepts_spelling(const xml::record_type& type, std::string_view name, std::string_view value)
    -> bool {
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
    // The old dispatcher tried the swap mappers before the FX one and the
    // scripted one after both, so the registry keeps that order.
    static const std::vector<instrument_projection> all = {
        {"rates", &read_rate_instrument, &write_rate_instrument},
        {"fx", &read_fx_instrument, &write_fx_instrument},
        {"scripted", &read_scripted_instrument, &write_scripted_instrument},
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

std::vector<xml::trade_import_item> portfolio_projection::map(const document& v) {
    const auto& type = *pf::portfolio_document().root;
    const auto root = xml::record_reader(type, &v);

    std::vector<xml::trade_import_item> r;
    for (const auto& trade : root.children("Trade")) {
        const auto trade_type = trade.spelling("TradeType");
        if (!trade_type)
            throw std::runtime_error("a trade the portfolio document states has no type");

        xml::trade_import_item item;
        item.ore_id = trade.text("id").value_or("");
        item.anchor.trade_type = *trade_type;
        item.envelope = read_envelope(trade);

        for (const auto& family : instrument_projections()) {
            auto instrument = family.read(trade, *trade_type);
            if (!instrument)
                continue;
            item.instrument = std::move(*instrument);
            break;
        }
        r.push_back(std::move(item));
    }
    return r;
}

portfolio_projection::document portfolio_projection::reverse(
    const std::vector<trading::messaging::trade_export_item>& items,
    const trading::domain::instrument_batch& instruments,
    const portfolio_reference_curves& reference_curves) {
    document doc;
    const auto& type = *pf::portfolio_document().root;
    xml::record_writer root(type, &doc);

    const auto* trade_type = xml::find_child_type(type, "Trade");
    if (trade_type == nullptr)
        throw std::runtime_error("the portfolio table has no Trade element");

    for (const auto& item : items) {
        if (!accepts_spelling(*trade_type, "TradeType", item.anchor.trade_type))
            continue;

        doc.trade.emplace_back();
        xml::record_writer trade(*trade_type, &doc.trade.back());
        trade.set_spelling("TradeType", item.anchor.trade_type, "trade type");
        if (!item.ore_id.empty())
            trade.set_text("id", item.ore_id);
        if (item.envelope)
            write_envelope(trade, *item.envelope);

        const auto instrument =
            trading::domain::rebuild_instrument(instruments, item.anchor.id);
        if (std::holds_alternative<std::monostate>(instrument))
            continue;

        for (const auto& family : instrument_projections())
            if (family.write(trade, item.anchor.trade_type, instrument, reference_curves))
                break;
    }

    root.state("Trade");
    return doc;
}

}
