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
#ifndef ORES_IAM_CLIENT_CLIENT_TENANT_SESSION_HPP
#define ORES_IAM_CLIENT_CLIENT_TENANT_SESSION_HPP

#include "ores.iam.client/export.hpp"
#include <chrono>
#include <string>

namespace ores::iam::client {

/**
 * @brief One tenant session a service holds, and when it must be entered again.
 *
 * A session is entered once and reused until it is close to its expiry, so the
 * service holds one identity for the work rather than asking for one per
 * request. The margin is capped at half the lifetime the entry answers, so a
 * short-lived session is still renewed before it runs out.
 */
class ORES_IAM_CLIENT_EXPORT tenant_session final {
public:
    explicit tenant_session(std::string party_id = {},
                            std::chrono::seconds refresh_margin = std::chrono::seconds{60});

    /**
     * @brief Takes the session the entry answered.
     *
     * @param token The session token.
     * @param lifetime_s How long the entry stated the session lasts.
     * @param now The instant the entry answered.
     */
    void entered(std::string token, int lifetime_s, std::chrono::system_clock::time_point now);

    /// Whether the session must be entered again at @p now.
    [[nodiscard]] bool needs_entry(std::chrono::system_clock::time_point now) const;

    [[nodiscard]] const std::string& token() const {
        return token_;
    }

    [[nodiscard]] const std::string& party_id() const {
        return party_id_;
    }

    /// How long before expiry the session is renewed.
    [[nodiscard]] std::chrono::seconds margin() const {
        return margin_;
    }

private:
    std::string party_id_;
    std::chrono::seconds margin_;
    std::string token_;
    std::chrono::system_clock::time_point expires_at_{};
};

}

#endif
