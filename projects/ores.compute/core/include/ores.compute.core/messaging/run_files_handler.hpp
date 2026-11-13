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
#ifndef ORES_COMPUTE_MESSAGING_RUN_FILES_HANDLER_HPP
#define ORES_COMPUTE_MESSAGING_RUN_FILES_HANDLER_HPP

#include "ores.compute.core/export.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>

namespace ores::compute::messaging {

/**
 * @brief Serves the packages one report run read and wrote.
 *
 * The tree is this service's own rows: the batch whose external reference is
 * the run, its workunits, each workunit's input package, and the package the
 * accepted result wrote. Composing it here is what stops every screen that
 * wants a run's files from searching for the batch and reading three
 * collections itself.
 *
 * The bytes are not this service's, so nothing here is unpacked: an archive
 * travels as the object path the row already states.
 */
class ORES_COMPUTE_CORE_EXPORT run_files_handler final {
public:
    run_files_handler(ores::nats::service::client& nats,
                      ores::database::context ctx,
                      std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Handles compute.v1.ops.list_run_files (request/reply).
     */
    void list_run_files(ores::nats::message msg);

private:
    inline static std::string_view logger_name = "ores.compute.messaging.run_files_handler";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
