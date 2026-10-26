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
#ifndef ORES_NATS_SERVICE_NATS_HANDLE_HPP
#define ORES_NATS_SERVICE_NATS_HANDLE_HPP

#include <memory>
#include <nats/nats.h>

namespace ores::nats::service {

/**
 * @brief Frees a cnats object with the library's own destroy function.
 */
template <typename T, void (*Destroy)(T*)>
struct nats_deleter {
    void operator()(T* p) const noexcept {
        if (p)
            Destroy(p);
    }
};

/**
 * @brief Sole owner of a cnats object: it is freed when the handle goes out of
 * scope, on every path, including a throw.
 *
 * Take ownership the moment cnats hands an object over, and pass @c get() to
 * calls that only borrow it. A raw pointer with a destroy call written beside
 * it is how a message came to be freed only on the error path.
 */
template <typename T, void (*Destroy)(T*)>
using nats_handle = std::unique_ptr<T, nats_deleter<T, Destroy>>;

using msg_handle = nats_handle<natsMsg, &natsMsg_Destroy>;
using pub_ack_handle = nats_handle<jsPubAck, &jsPubAck_Destroy>;

}

#endif
