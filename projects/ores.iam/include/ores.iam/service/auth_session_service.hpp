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
#ifndef ORES_IAM_SERVICE_AUTH_SESSION_SERVICE_HPP
#define ORES_IAM_SERVICE_AUTH_SESSION_SERVICE_HPP

#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>
#include "ores.iam/domain/session.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::iam::service {

/**
 * @brief In-memory JWT session tracker.
 *
 * Maps Bearer tokens to session data for fast in-process auth checks.
 */
class auth_session_service {
public:
    void add_session(const std::string& token, ores::iam::domain::session session);
    void remove_session(const std::string& token);
    std::optional<ores::iam::domain::session>
    find_session(const std::string& token) const;
    std::vector<ores::iam::domain::session> get_all_sessions() const;

private:
    inline static std::string_view logger_name =
        "ores.iam.service.auth_session_service";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    mutable std::mutex mutex_;
    std::unordered_map<std::string, ores::iam::domain::session> sessions_;
};

}

#endif
