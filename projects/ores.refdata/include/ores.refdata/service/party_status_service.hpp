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
#ifndef ORES_REFDATA_SERVICE_PARTY_STATUS_SERVICE_HPP
#define ORES_REFDATA_SERVICE_PARTY_STATUS_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/party_status.hpp"
#include "ores.refdata/repository/party_status_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing party statuses.
 */
class party_status_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.party_status_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit party_status_service(context ctx);

    std::vector<domain::party_status> list_statuses();

    std::optional<domain::party_status>
    find_status(const std::string& code);

    void save_status(const domain::party_status& status);

    void save_statuses(const std::vector<domain::party_status>& statuses);

    void remove_status(const std::string& code);

    void remove_statuses(const std::vector<std::string>& codes);

    std::vector<domain::party_status>
    get_status_history(const std::string& code);

private:
    context ctx_;
    repository::party_status_repository repo_;
};

}

#endif
