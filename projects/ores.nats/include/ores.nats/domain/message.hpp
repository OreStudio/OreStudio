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
#ifndef ORES_NATS_DOMAIN_MESSAGE_HPP
#define ORES_NATS_DOMAIN_MESSAGE_HPP

#include <cstddef>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::nats {

/**
 * @brief Answers a durable delivery: ack, nak or term.
 *
 * A core NATS message has no such handle, because there is nothing to
 * acknowledge. A JetStream message carries one, and the handler that decides
 * the outcome answers through it. The transport's own backstop naks a message
 * left unanswered, so an undecided delivery is redelivered rather than
 * silently acknowledged.
 */
class ack_handle {
public:
    virtual ~ack_handle() = default;

    /// The work is committed; the message is delivered no more.
    virtual void accept() = 0;
    /// The work failed; deliver the message again.
    virtual void retry() = 0;
    /// The message can never be handled; stop delivering it.
    virtual void reject() = 0;
};

/**
 * @brief A received NATS message.
 *
 * No cnats types appear here. All data is copied out of the cnats message
 * before the callback returns so the cnats message can be destroyed
 * immediately on the cnats internal thread.
 */
struct message {
    /**
     * @brief The subject the message was published to.
     */
    std::string subject;

    /**
     * @brief The reply-to subject, empty for one-way publishes.
     *
     * Services write their response to this subject.
     */
    std::string reply_subject;

    /**
     * @brief The message payload bytes.
     *
     * Typically a MessagePack-encoded domain struct.
     */
    std::vector<std::byte> data;

    /**
     * @brief NATS message headers (NATS 2.2+).
     *
     * The @c Authorization header carries the JWT bearer token for
     * per-request authentication: @c "Authorization" -> @c "Bearer <token>".
     */
    std::unordered_map<std::string, std::string> headers;

    /**
     * @brief The delivery's acknowledgement handle, or nothing for core NATS.
     *
     * Held by shared handle so the message stays cheap to pass by value, which
     * is how every handler receives it. An @c ack_guard answers on the
     * handler's behalf; the handle itself accepts only the first answer.
     */
    std::shared_ptr<ack_handle> ack;
};

/**
 * @brief RAII guard for a durable delivery, defaulting to a retry.
 *
 * Declared at the top of a handler, it answers the message on the handler's
 * behalf when the handler answers nothing itself. The destructor runs @c retry,
 * so a path that throws or returns early is redelivered rather than
 * acknowledged as done — the failure mode that defeats at-least-once delivery.
 *
 * @code
 * void provision_step(ores::nats::message msg) {
 *     ores::nats::ack_guard ack(msg);
 *     // ... work ...
 *     ack.accept();   // committed
 * }
 * @endcode
 *
 * A message with no handle — every core NATS message — makes every method a
 * no-op, so one handler shape serves both.
 */
class ack_guard {
public:
    explicit ack_guard(const message& msg) noexcept
        : handle_(msg.ack.get()) {}

    ~ack_guard() {
        if (handle_ && !answered_)
            handle_->retry();
    }

    ack_guard(const ack_guard&) = delete;
    ack_guard& operator=(const ack_guard&) = delete;

    /// @return true when this message can be answered at all.
    [[nodiscard]] bool has_handle() const noexcept {
        return handle_ != nullptr;
    }

    void accept() noexcept {
        answer(&ack_handle::accept);
    }

    void retry() noexcept {
        answer(&ack_handle::retry);
    }

    void reject() noexcept {
        answer(&ack_handle::reject);
    }

private:
    void answer(void (ack_handle::*verb)()) noexcept {
        if (handle_ && !answered_) {
            answered_ = true;
            (handle_->*verb)();
        }
    }

    ack_handle* handle_;
    bool answered_ = false;
};

/**
 * @brief Callback type for incoming NATS messages.
 */
using message_handler = std::function<void(message)>;

/**
 * @brief Reinterprets a string's character data as a read-only byte span.
 *
 * Use this when calling request_sync() or publish() with a JSON/text payload
 * that is already held in a std::string or std::string_view, so callers do
 * not need to scatter reinterpret_cast throughout service code.
 */
inline std::span<const std::byte> as_bytes(std::string_view s) noexcept {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

}

#endif
