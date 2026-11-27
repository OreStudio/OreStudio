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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_VAULT_CORE_SECURITY_CHALLENGE_REGISTRY_HPP
#define ORES_VAULT_CORE_SECURITY_CHALLENGE_REGISTRY_HPP

#include <chrono>
#include <cstddef>
#include <mutex>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>

namespace ores::vault::security {

/**
 * @brief The challenges the vault has issued and not yet seen answered.
 *
 * A challenge is a nonce issued to one consumer for one purpose, good once and
 * for a short time. It lives in the process rather than in a table: losing it
 * to a restart costs a caller one retry, and a challenge that outlived the
 * process would be a nonce nobody is waiting for.
 *
 * One instance is held by the process that serves the operations, because the
 * custody service is built per request and this state is not.
 */
class challenge_registry {
public:
    /// The life a challenge is given: long enough for one exchange, short
    /// enough that a captured nonce is worthless before it is used.
    static constexpr std::chrono::seconds default_life{60};

    /// The most challenges the vault holds at once. Issuing one costs no
    /// credential, so the number outstanding is bounded rather than left to
    /// whoever asks: at a minute's life this is the ceiling on the memory one
    /// caller can occupy, and a burst larger than this is a caller retrying.
    static constexpr std::size_t max_live{4096};

    /**
     * @brief Records a nonce against a challenge and a consumer.
     *
     * @return Whether the challenge was recorded, which it is not when the
     * registry already holds as many as it will.
     */
    bool issue(const std::string& challenge_id, const std::string& consumer, std::string nonce) {
        std::lock_guard<std::mutex> lock(mutex_);
        forget_expired();
        if (held_.size() >= max_live)
            return false;
        held_[challenge_id] =
            held{consumer, nonce, std::chrono::steady_clock::now() + default_life};
        return true;
    }

    /**
     * @brief Retires a challenge and returns the nonce it was issued with.
     *
     * A challenge is consumed whether or not the answer matches, so a wrong
     * answer costs the caller a fresh exchange rather than another guess at the
     * nonce.
     *
     * @return The nonce, or nothing when the challenge is unknown, expired or
     * belongs to another consumer.
     */
    [[nodiscard]] std::optional<std::string> redeem(const std::string& challenge_id,
                                                    const std::string& consumer) {
        std::lock_guard<std::mutex> lock(mutex_);
        forget_expired();

        const auto found = held_.find(challenge_id);
        if (found == held_.end())
            return std::nullopt;

        auto entry = std::move(found->second);
        held_.erase(found);
        if (entry.consumer != consumer)
            return std::nullopt;
        return entry.nonce;
    }

private:
    struct held {
        std::string consumer;
        std::string nonce;
        std::chrono::steady_clock::time_point expires;
    };

    void forget_expired() {
        const auto now = std::chrono::steady_clock::now();
        for (auto it = held_.begin(); it != held_.end();) {
            if (it->second.expires <= now)
                it = held_.erase(it);
            else
                ++it;
        }
    }

    std::mutex mutex_;
    std::unordered_map<std::string, held> held_;
};

}

#endif
