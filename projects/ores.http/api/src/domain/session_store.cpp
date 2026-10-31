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
#include "ores.http.api/domain/session_store.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <array>
#include <cstdint>
#include <random>

namespace ores::http::domain {

namespace {

/// 256 bits of identifier, rendered as hex.
std::string mint_identifier() {
    static thread_local std::random_device source;
    static constexpr auto digits = std::string_view("0123456789abcdef");

    std::array<std::uint8_t, 32> bytes{};
    for (std::size_t i = 0; i < bytes.size(); i += 4) {
        const auto drawn = source();
        bytes[i] = static_cast<std::uint8_t>(drawn & 0xff);
        bytes[i + 1] = static_cast<std::uint8_t>((drawn >> 8) & 0xff);
        bytes[i + 2] = static_cast<std::uint8_t>((drawn >> 16) & 0xff);
        bytes[i + 3] = static_cast<std::uint8_t>((drawn >> 24) & 0xff);
    }

    std::string identifier;
    identifier.reserve(bytes.size() * 2);
    for (const auto byte : bytes) {
        identifier.push_back(digits[byte >> 4]);
        identifier.push_back(digits[byte & 0x0f]);
    }
    return identifier;
}

}

session_store::session_store(std::chrono::seconds ttl, clock now)
    : ttl_(ttl)
    , now_(std::move(now)) {}

std::string session_store::key_of(const std::string& identifier) {
    return utility::crypto::sha256::hex_digest(identifier);
}

bool session_store::expired(const entry& kept) const {
    return now_() >= kept.expires;
}

std::string session_store::open(session_record record) {
    const auto identifier = mint_identifier();
    auto kept = entry{std::move(record), {}};
    kept.record.last_used = now_();
    if (kept.record.created == std::chrono::system_clock::time_point{})
        kept.record.created = kept.record.last_used;
    kept.expires = kept.record.last_used + ttl_;

    const std::lock_guard lock(mutex_);
    entries_.insert_or_assign(key_of(identifier), std::move(kept));
    return identifier;
}

std::optional<session_record> session_store::touch(const std::string& identifier) {
    const std::lock_guard lock(mutex_);

    const auto found = entries_.find(key_of(identifier));
    if (found == entries_.end())
        return std::nullopt;
    if (expired(found->second)) {
        entries_.erase(found);
        return std::nullopt;
    }

    found->second.record.last_used = now_();
    found->second.expires = found->second.record.last_used + ttl_;
    return found->second.record;
}

std::optional<session_record> session_store::find(const std::string& identifier) const {
    const std::lock_guard lock(mutex_);

    const auto found = entries_.find(key_of(identifier));
    if (found == entries_.end() || expired(found->second))
        return std::nullopt;
    return found->second.record;
}

bool session_store::close(const std::string& identifier) {
    const std::lock_guard lock(mutex_);
    return entries_.erase(key_of(identifier)) > 0;
}

std::size_t session_store::evict_expired() {
    const std::lock_guard lock(mutex_);

    auto removed = std::size_t{0};
    for (auto it = entries_.begin(); it != entries_.end();) {
        if (expired(it->second)) {
            it = entries_.erase(it);
            ++removed;
        } else {
            ++it;
        }
    }
    return removed;
}

std::optional<std::string> session_store::rotate(const std::string& identifier) {
    const std::lock_guard lock(mutex_);

    auto found = entries_.find(key_of(identifier));
    if (found == entries_.end())
        return std::nullopt;

    auto kept = std::move(found->second);
    entries_.erase(found);

    if (expired(kept))
        return std::nullopt;

    const auto replacement = mint_identifier();
    kept.record.last_used = now_();
    kept.expires = kept.record.last_used + ttl_;
    entries_.insert_or_assign(key_of(replacement), std::move(kept));
    return replacement;
}

std::size_t session_store::size() const {
    const std::lock_guard lock(mutex_);
    return entries_.size();
}

}
