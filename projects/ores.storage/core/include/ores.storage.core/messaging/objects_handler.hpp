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
#ifndef ORES_STORAGE_CORE_MESSAGING_OBJECTS_HANDLER_HPP
#define ORES_STORAGE_CORE_MESSAGING_OBJECTS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.storage.core/export.hpp"
#include "ores.storage.core/filesystem/local_store.hpp"
#include <optional>

namespace ores::storage::messaging {

/**
 * @brief Serves the object operation set on the bus.
 *
 * One method per subject the operation model declares: put, get, delete and
 * list. Each one authenticates the caller, checks the permission code for its
 * own operation, and only then touches the store, so a read is never admitted
 * by the code a write uses.
 *
 * The store is the same =local_store= the HTTP routes call. That is what makes
 * the two interfaces peers rather than two implementations: the same bucket and
 * key leave the same object, and the same object raises the same absence.
 *
 * An operation model generates no handler, which is why this class is
 * hand-written; it exists to answer subjects the model declares and nothing
 * else.
 */
class ORES_STORAGE_CORE_EXPORT objects_handler final {
public:
    objects_handler(ores::nats::service::client& nats,
                    ores::database::context base_ctx,
                    filesystem::local_store store,
                    std::optional<ores::security::jwt::jwt_authenticator> verifier);

    objects_handler(const objects_handler&) = delete;
    objects_handler& operator=(const objects_handler&) = delete;

    /**
     * @brief Creates or replaces one object.
     */
    void put(ores::nats::message msg);

    /**
     * @brief Reads one object's metadata, and its value when asked for.
     */
    void get(ores::nats::message msg);

    /**
     * @brief Removes one object.
     */
    void remove(ores::nats::message msg);

    /**
     * @brief Answers a page of a bucket, filtered by key prefix.
     */
    void list(ores::nats::message msg);

private:
    inline static std::string_view logger_name = "ores.storage.messaging.objects_handler";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    /**
     * @brief Builds the caller's context and checks one operation's code.
     *
     * Replies with the failure itself when either step fails, so a caller can
     * simply return when this answers nothing.
     */
    std::optional<ores::database::context>
    authorise(const ores::nats::message& msg, std::string_view required_permission,
              std::string_view operation_name);

    ores::nats::service::client& nats_;
    ores::database::context base_ctx_;
    filesystem::local_store store_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
