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
#ifndef ORES_INBOX_CORE_SERVICE_APPROVAL_ANNOUNCER_HPP
#define ORES_INBOX_CORE_SERVICE_APPROVAL_ANNOUNCER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/approval_kind.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include <string>

namespace ores::inbox::service {

/**
 * @brief Tells the people who may decide a request that it waits.
 *
 * Any component that raises a request calls this once the request is
 * written, so every kind tells its deciders the same way. Telling is never
 * the operation: a failure is logged, and the request stands.
 */
class ORES_INBOX_CORE_EXPORT approval_announcer {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance = ores::logging::make_logger("ores.inbox.service.approval_announcer");
        return instance;
    }

public:
    explicit approval_announcer(ores::database::context ctx);

    /**
     * @brief Tells the deciders whose turn it is.
     *
     * A request of a kind with one decider permission tells its holders. A
     * request that names parts tells the holders of the parts that are open:
     * those of the earliest answer order that has not yet approved.
     */
    void tell_open_parts(approval_lifecycle& lifecycle,
                         const domain::approval_kind& kind,
                         const domain::approval_request& raised);

    /**
     * @brief Tells the holders of a permission, other than the person who
     * asked, that a request waits.
     */
    void tell_deciders(const domain::approval_kind& kind,
                       const domain::approval_request& raised,
                       const std::string& permission_code);

private:
    ores::database::context ctx_;
};

}

#endif
