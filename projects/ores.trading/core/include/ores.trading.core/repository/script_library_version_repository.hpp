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
#ifndef ORES_TRADING_CORE_REPOSITORY_SCRIPT_LIBRARY_VERSION_REPOSITORY_HPP
#define ORES_TRADING_CORE_REPOSITORY_SCRIPT_LIBRARY_VERSION_REPOSITORY_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.logging/logger.hpp"
#include "ores.trading.api/messaging/script_library_operation_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include <string_view>
#include <vector>

namespace ores::trading::repository {

/**
 * @brief The rows of stated versions of library scripts.
 *
 * A version is the script's row at that version together with every row beneath
 * the script that carries it as its script version, whether those rows are
 * current or closed. A caller reads only its own tenant's scripts and the
 * system tenant's, as for every other read of the library.
 */
class ORES_TRADING_CORE_EXPORT script_library_version_repository {
private:
    inline static std::string_view logger_name =
        "ores.trading.repository.script_library_version_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Reads each pin once, and names the pins the store does not hold.
     *
     * The rows go into the response's arrays and the absent pins into its
     * missing list. The response's success and message are left to the caller.
     */
    void read(const context& ctx,
              const std::vector<messaging::script_pin>& pins,
              messaging::export_script_library_response& out);
};

}

#endif
