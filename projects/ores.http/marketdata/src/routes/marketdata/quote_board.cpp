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
#include "ores.http/routes/marketdata/quote_board.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.api/domain/asset_class_authorities.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <algorithm>
#include <optional>
#include <variant>

namespace ores::http::routes::marketdata {

namespace {

namespace datum = ores::marketdata::datum;
using ores::marketdata::messaging::market_tick;

/// The provider and the feed a feed subject names, or nothing for a subject that names none.
std::optional<quote_board::feed_key> feed_of(std::string_view subject) {
    const auto base = std::string(market_tick::nats_subject) + ".";
    const auto at = subject.find(base);
    if (at == std::string_view::npos)
        return std::nullopt;
    const auto rest = subject.substr(at + base.size());
    const auto dot = rest.find('.');
    if (dot == std::string_view::npos)
        return std::nullopt;
    const auto feed_end = rest.find('.', dot + 1);
    if (feed_end == std::string_view::npos)
        return std::nullopt;
    const auto provider = ores::marketdata::domain::subject_token_text(rest.substr(0, dot));
    if (!provider)
        return std::nullopt;
    return quote_board::feed_key{*provider, std::string(rest.substr(dot + 1, feed_end - dot - 1))};
}

/// The words of @p fields that hold a value, joined by a space.
std::string words_of(const datum::market_datum& d, datum::field_role role, bool subject_first) {
    const auto& row = datum::schema_of(d.type());
    std::string out;
    const auto append = [&](const datum::field_value& fv) {
        if (std::holds_alternative<datum::none_t>(fv.held))
            return;
        if (!out.empty())
            out.push_back(' ');
        out.append(datum::text_of(fv.held));
    };
    if (subject_first)
        for (const auto& fv : d.fields())
            if (fv.name == row.subject)
                append(fv);
    for (const auto& fv : d.fields()) {
        if (subject_first && fv.name == row.subject)
            continue;
        const auto spec = std::ranges::find(row.fields, fv.name, &datum::field_spec::name);
        if (spec != row.fields.end() && spec->role == role)
            append(fv);
    }
    return out;
}

/// @p q with the labels its URI gives, or nothing for a URI the codec cannot read.
std::optional<quote> labelled(quote q) {
    const auto d = datum::oresmd_uri_codec::read(q.uri);
    if (!d)
        return std::nullopt;
    const auto series = datum::series_of(*d);
    const auto series_uri = datum::oresmd_uri_codec::write(series);
    if (!series_uri)
        return std::nullopt;
    q.instrument_type = datum::oresmd_uri_codec::instrument_spelling(d->type());
    const auto authority = datum::name_of(datum::schema_of(d->type()).asset);
    q.asset_class =
        std::string(ores::marketdata::domain::asset_class_for_authority(authority).value_or(""));
    q.series_uri = *series_uri;
    q.series = words_of(series, datum::field_role::identity, true);
    q.point = words_of(*d, datum::field_role::coordinate, false);
    return q;
}

}

quote_board::quote_board(subscriber subscribe)
    : subscribe_(std::move(subscribe)) {}

quote_board::subscriber quote_board::nats_subscriber(ores::nats::service::nats_client& session) {
    return [&session](const std::string& subject, tick_handler on_tick) -> std::shared_ptr<void> {
        auto listening = session.transport().subscribe(subject, [on_tick](ores::nats::message msg) {
            const auto tick = ores::nats::default_wire_codec().decode<market_tick>(msg.data);
            if (tick)
                on_tick(msg.subject, *tick);
        });
        return std::make_shared<ores::nats::service::subscription>(std::move(listening));
    };
}

void quote_board::start() {
    // The board's own lock is not held here: a transport may hand a tick to take() while the
    // subscription is being made.
    std::call_once(started_, [this] {
        listening_ = subscribe_(
            std::string(market_tick::nats_subject) + ".>",
            [this](const std::string& subject, const market_tick& tick) { take(subject, tick); });
    });
}

void quote_board::take(const std::string& subject, const market_tick& tick) {
    const auto feed = feed_of(subject);
    if (!feed)
        return;
    std::lock_guard lock(mutex_);
    const auto key = std::make_pair(*feed, tick.oresmd_uri);
    auto found = quotes_.find(key);
    if (found == quotes_.end()) {
        if (quotes_.size() >= capacity)
            return;
        quote fresh;
        fresh.provider = feed->first;
        fresh.feed = feed->second;
        fresh.uri = tick.oresmd_uri;
        fresh.previous = tick.value;
        auto named = labelled(std::move(fresh));
        if (!named)
            return;
        found = quotes_.emplace(key, std::move(*named)).first;
    } else {
        found->second.previous = found->second.value;
    }
    auto& q = found->second;
    q.source = tick.source;
    q.value = tick.value;
    q.at = tick.observation_time;
    q.sequence = ++sequence_;
}

std::vector<quote> quote_board::after(std::uint64_t sequence,
                                      const std::set<feed_key>& feeds) const {
    std::vector<quote> out;
    {
        std::lock_guard lock(mutex_);
        for (const auto& [key, q] : quotes_)
            if (q.sequence > sequence && feeds.contains(key.first))
                out.push_back(q);
    }
    std::ranges::sort(out, {}, &quote::sequence);
    return out;
}

std::uint64_t quote_board::sequence() const {
    std::lock_guard lock(mutex_);
    return sequence_;
}

std::map<quote_board::feed_key, std::pair<std::string, std::chrono::system_clock::time_point>>
quote_board::feeds() const {
    std::map<feed_key, std::pair<std::string, std::chrono::system_clock::time_point>> out;
    std::lock_guard lock(mutex_);
    for (const auto& [key, q] : quotes_) {
        auto& entry = out[key.first];
        if (q.at >= entry.second)
            entry = {q.source, q.at};
    }
    return out;
}

}
