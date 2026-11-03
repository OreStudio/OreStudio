/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 *
 */
#ifndef ORES_TRADING_CORE_SERVICE_BOOK_STATUS_PORT_HPP
#define ORES_TRADING_CORE_SERVICE_BOOK_STATUS_PORT_HPP

#include <boost/uuid/uuid.hpp>
#include <string_view>

namespace ores::trading::service {

/**
 * @brief Whether a book is open, as another service knows it.
 *
 * A book's status is reference data: the refdata service owns it and this
 * component does not store it. A booking must land in a book that is open, and
 * that is a rule about the book, so asking is the only honest way to know it.
 *
 * The interface is a port because the caller decides how to ask: the service
 * process asks over the message bus, and a test hands in its own answer. The
 * booking service is a database service with no bus of its own, and it must stay
 * one.
 */
class book_status_port {
public:
    /** What the port can say about a book. */
    enum class state {
        open,
        not_open,
        unknown,
    };

    virtual ~book_status_port() = default;

    /**
     * @brief The state of the book a booking names, asked as @p actor_token.
     *
     * `unknown` means the answer could not be had: the book was not found, or
     * the ask was refused, or the other service did not answer. A caller that
     * cannot tell a frozen book from an unreadable one must not pretend it is
     * open, so the booking service refuses on `not_open` and carries on with an
     * `unknown`, which it logs. A booking refused because refdata was busy would
     * be a worse fault than the one this rule prevents.
     */
    [[nodiscard]] virtual state of(const boost::uuids::uuid& book_id,
                                   std::string_view actor_token) const = 0;
};

/** A port that knows nothing, for callers that cannot ask. */
class unknown_book_status final : public book_status_port {
public:
    [[nodiscard]] state of(const boost::uuids::uuid&, std::string_view) const override {
        return state::unknown;
    }
};

}

#endif
