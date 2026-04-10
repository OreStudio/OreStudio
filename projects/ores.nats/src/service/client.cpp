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
#include "ores.nats/service/client.hpp"

#include <atomic>
#include "ores.nats/service/jetstream_admin.hpp"
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>
#include "ores.logging/make_logger.hpp"

#include <nats/nats.h>

#include <boost/asio/any_io_executor.hpp>
#include <boost/asio/as_tuple.hpp>
#include <boost/asio/experimental/channel.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/error_code.hpp>

#include "ores.nats/service/subscription.hpp"
#include "ores.nats/service/nats_connect_error.hpp"

namespace ores::nats::service {

using namespace ores::logging;

namespace {
    inline static std::string_view logger_name = "ores.nats.service.client";
    static auto& lg() {
        static auto instance = make_logger(logger_name);
        return instance;
    }
}

// ---------------------------------------------------------------------------
// Subscription callback closure
// ---------------------------------------------------------------------------

struct sub_closure {
    message_handler handler;
};

// ---------------------------------------------------------------------------
// subscription::impl
// ---------------------------------------------------------------------------

struct subscription::impl {
    natsSubscription* sub = nullptr;
    std::unique_ptr<sub_closure> closure; // must outlive sub
};

// ---------------------------------------------------------------------------
// client::impl
// ---------------------------------------------------------------------------

struct client::impl {
    config::nats_options opts;
    natsConnection* conn = nullptr;
    jsCtx* js = nullptr;
    std::atomic<bool> connected{false};
};

// ---------------------------------------------------------------------------
// Internal helpers (anonymous namespace)
// ---------------------------------------------------------------------------

namespace {

// NATS async error handler: forwards connection/subscription errors to
// Boost.Log instead of the library's default stderr print.
// NATS_CONNECTION_CLOSED is expected during orderly drain — suppress it.
void on_conn_error(natsConnection* /*nc*/, natsSubscription* /*sub*/,
                   natsStatus err, void* /*closure*/) {
    if (err == NATS_CONNECTION_CLOSED)
        return;
    try {
        BOOST_LOG_SEV(lg(), warn) << "NATS error: " << natsStatus_GetText(err);
    } catch (...) {
        // Boost.Log may already be torn down on a NATS internal thread.
    }
}

message extract_message(natsMsg* msg) {
    message m;

    if (const char* s = natsMsg_GetSubject(msg))
        m.subject = s;
    if (const char* r = natsMsg_GetReply(msg))
        m.reply_subject = r;

    const int data_len = natsMsg_GetDataLength(msg);
    if (data_len > 0) {
        const auto* p = reinterpret_cast<const std::byte*>(natsMsg_GetData(msg));
        m.data.assign(p, p + data_len);
    }

    // Extract all headers; cnats allocates the keys array but the key strings
    // themselves are owned by the message — only free the array, not the keys.
    const char** keys = nullptr;
    int key_count = 0;
    if (natsMsgHeader_Keys(msg, &keys, &key_count) == NATS_OK && keys) {
        for (int i = 0; i < key_count; ++i) {
            const char* val = nullptr;
            if (natsMsgHeader_Get(msg, keys[i], &val) == NATS_OK && val)
                m.headers[keys[i]] = val;
        }
        // NOLINTNEXTLINE(cppcoreguidelines-no-malloc,hicpp-no-malloc)
        free(keys);
    }

    return m;
}

// cnats message callback used by all subscribe variants.
// JetStream subscriptions auto-ack by default (ManualAck is false).
//
// IMPORTANT: This is a C callback running on a NATS library thread.
// Exceptions must NOT propagate out — they would cross a C frame boundary
// and call std::terminate(), killing the service without any shutdown log.
void on_msg(natsConnection*, natsSubscription*, natsMsg* msg, void* ud) {
    auto* cl = static_cast<sub_closure*>(ud);
    message m = extract_message(msg);
    natsMsg_Destroy(msg);
    const auto subject = m.subject; // save before move
    try {
        cl->handler(std::move(m));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Unhandled exception in NATS message handler for subject '"
            << subject << "': " << e.what();
    } catch (...) {
        BOOST_LOG_SEV(lg(), error)
            << "Unknown exception in NATS message handler for subject '"
            << subject << "'";
    }
}

// Build a natsMsg* with data and optional headers.
// Caller owns the result and must call natsMsg_Destroy.
natsMsg* make_msg(std::string_view subject,
    std::span<const std::byte> data,
    const std::unordered_map<std::string, std::string>& headers,
    const char* reply = nullptr) {

    natsMsg* msg = nullptr;
    const natsStatus s = natsMsg_Create(&msg,
        std::string(subject).c_str(),
        reply,
        reinterpret_cast<const char*>(data.data()),
        static_cast<int>(data.size()));

    if (s != NATS_OK)
        throw std::runtime_error(
            std::string("natsMsg_Create failed: ") + natsStatus_GetText(s));

    for (const auto& [k, v] : headers)
        natsMsgHeader_Set(msg, k.c_str(), v.c_str());

    return msg;
}

} // namespace

// ---------------------------------------------------------------------------
// subscription — methods defined here because subscription::impl lives here
// ---------------------------------------------------------------------------

subscription::subscription(std::unique_ptr<impl> i) : impl_(std::move(i)) {}

subscription::~subscription() {
    if (!impl_ || !impl_->sub)
        return;
    natsSubscription_Unsubscribe(impl_->sub);
    natsSubscription_Destroy(impl_->sub);
    impl_->sub = nullptr;
    // impl_->closure destroyed with impl_
}

subscription::subscription(subscription&&) noexcept = default;
subscription& subscription::operator=(subscription&&) noexcept = default;

std::string subscription::subject() const {
    if (!impl_ || !impl_->sub)
        return {};
    const char* s = natsSubscription_GetSubject(impl_->sub);
    return s ? s : std::string{};
}

void subscription::drain() {
    if (impl_ && impl_->sub)
        natsSubscription_Drain(impl_->sub);
}

// ---------------------------------------------------------------------------
// client
// ---------------------------------------------------------------------------

client::client(config::nats_options opts) : impl_(std::make_unique<impl>()) {
    impl_->opts = std::move(opts);
}

client::~client() {
    disconnect();
}

void client::connect() {
    if (impl_->connected.load(std::memory_order_acquire))
        return;

    if (impl_->opts.subject_prefix.empty())
        throw std::runtime_error("NATS subject_prefix must be set in configuration");

    natsOptions* opts = nullptr;
    natsOptions_Create(&opts);
    natsOptions_SetURL(opts, impl_->opts.url.c_str());
    natsOptions_SetErrorHandler(opts, on_conn_error, nullptr);

    if (!impl_->opts.tls_ca_cert.empty()) {
        // Validate TLS files exist before attempting connection so we can log
        // a clear diagnostic instead of a cryptic SSL handshake error.
        auto check_file = [](const std::string& path, const char* role) {
            if (!std::filesystem::exists(path))
                throw std::runtime_error(
                    std::string("NATS TLS ") + role + " not found: " + path);
        };
        check_file(impl_->opts.tls_ca_cert,      "CA certificate");
        check_file(impl_->opts.tls_client_cert,  "client certificate");
        check_file(impl_->opts.tls_client_key,   "client key");

        natsOptions_SetSecure(opts, true);
        natsOptions_LoadCATrustedCertificates(
            opts, impl_->opts.tls_ca_cert.c_str());
        natsOptions_LoadCertificatesChain(
            opts,
            impl_->opts.tls_client_cert.c_str(),
            impl_->opts.tls_client_key.c_str());
        BOOST_LOG_SEV(lg(), info)
            << "mTLS enabled (ca=" << impl_->opts.tls_ca_cert
            << ", cert=" << impl_->opts.tls_client_cert << ")";
    }

    natsStatus s = natsConnection_Connect(&impl_->conn, opts);
    natsOptions_Destroy(opts);
    if (s != NATS_OK) {
        nats_error_kind kind;
        switch (s) {
            case NATS_SSL_ERROR:
            case NATS_SECURE_CONNECTION_WANTED:
            case NATS_SECURE_CONNECTION_REQUIRED:
                kind = nats_error_kind::tls_error;
                break;
            case NATS_NO_SERVER:
            case NATS_IO_ERROR:
                kind = nats_error_kind::server_unreachable;
                break;
            case NATS_TIMEOUT:
                kind = nats_error_kind::connection_timeout;
                break;
            default:
                kind = nats_error_kind::other;
                break;
        }
        throw nats_connect_error(kind,
            std::string("NATS connect failed: ") + natsStatus_GetText(s));
    }

    // Note: natsConnection_JetStream(jsCtx**, natsConnection*, jsOptions*)
    s = natsConnection_JetStream(&impl_->js, impl_->conn, nullptr);
    if (s != NATS_OK) {
        natsConnection_Close(impl_->conn);
        natsConnection_Destroy(impl_->conn);
        impl_->conn = nullptr;
        throw std::runtime_error(
            std::string("NATS JetStream init failed: ") + natsStatus_GetText(s));
    }

    impl_->connected.store(true, std::memory_order_release);
}

void client::disconnect() {
    if (!impl_->connected.exchange(false, std::memory_order_acq_rel))
        return;

    if (impl_->js) {
        jsCtx_Destroy(impl_->js);
        impl_->js = nullptr;
    }
    if (impl_->conn) {
        natsConnection_Close(impl_->conn);
        natsConnection_Destroy(impl_->conn);
        impl_->conn = nullptr;
    }
}

bool client::is_connected() const noexcept {
    return impl_->connected.load(std::memory_order_acquire);
}

std::string client::make_subject(std::string_view relative) const {
    // NATS inbox subjects (_INBOX.*) are created by the server for
    // request/reply and must never carry a user-defined prefix.
    if (relative.starts_with("_INBOX"))
        return std::string(relative);
    return impl_->opts.subject_prefix + '.' + std::string(relative);
}

std::string client::make_stream_name(std::string_view logical_suffix) const {
    // JetStream stream names cannot contain dots — replace with underscores.
    // Result is all-lowercase: e.g. "ores.dev.local2" + "workflow"
    //   → "ores_dev_local2_workflow"
    std::string name = impl_->opts.subject_prefix;
    std::replace(name.begin(), name.end(), '.', '_');
    if (!logical_suffix.empty()) {
        name += '_';
        name += logical_suffix;
    }
    return name;
}

void client::publish(std::string_view subject,
    std::span<const std::byte> data,
    std::unordered_map<std::string, std::string> headers) {

    natsMsg* msg = make_msg(make_subject(subject), data, headers);
    const natsStatus s = natsConnection_PublishMsg(impl_->conn, msg);
    natsMsg_Destroy(msg);
    if (s != NATS_OK)
        throw std::runtime_error(
            std::string("NATS publish failed: ") + natsStatus_GetText(s));
}

message client::request_sync(std::string_view subject,
    std::span<const std::byte> data,
    std::unordered_map<std::string, std::string> headers,
    std::chrono::milliseconds timeout) {

    natsMsg* req = make_msg(make_subject(subject), data, headers);
    natsMsg* reply = nullptr;
    const natsStatus s = natsConnection_RequestMsg(
        &reply, impl_->conn, req, static_cast<int64_t>(timeout.count()));
    natsMsg_Destroy(req);

    if (s != NATS_OK) {
        if (s == NATS_TIMEOUT)
            throw std::runtime_error("NATS request timed out");
        if (s == NATS_NO_RESPONDERS)
            throw nats_connect_error(nats_error_kind::services_unavailable,
                "NATS request failed: no service is handling subject '"
                + std::string(make_subject(subject)) + "'");
        throw std::runtime_error(
            std::string("NATS request failed: ") + natsStatus_GetText(s));
    }

    message m = extract_message(reply);
    natsMsg_Destroy(reply);
    return m;
}

boost::asio::awaitable<message> client::request(std::string_view subject,
    std::span<const std::byte> data,
    std::unordered_map<std::string, std::string> headers,
    std::chrono::milliseconds timeout) {

    using chan_t = boost::asio::experimental::channel<
        boost::asio::any_io_executor,
        void(boost::system::error_code, message)>;

    auto ex = co_await boost::asio::this_coro::executor;
    auto chan = std::make_shared<chan_t>(ex, 1);

    std::string subj(subject);
    std::vector<std::byte> payload(data.begin(), data.end());

    std::thread([this, subj, payload, hdrs = std::move(headers), timeout, chan]() mutable {
        try {
            auto result = request_sync(subj, payload, hdrs, timeout);
            chan->try_send(boost::system::error_code{}, std::move(result));
        } catch (const std::exception&) {
            chan->try_send(
                boost::system::error_code{boost::asio::error::operation_aborted},
                message{});
        }
    }).detach();

    auto [ec, msg] = co_await chan->async_receive(
        boost::asio::as_tuple(boost::asio::use_awaitable));

    if (ec)
        throw std::runtime_error("NATS async request failed: " + ec.message());

    co_return std::move(msg);
}

subscription client::subscribe(std::string_view subject, message_handler handler) {
    auto cl = std::make_unique<sub_closure>();
    cl->handler = std::move(handler);

    natsSubscription* sub = nullptr;
    const natsStatus s = natsConnection_Subscribe(
        &sub, impl_->conn, make_subject(subject).c_str(), on_msg, cl.get());

    if (s != NATS_OK)
        throw std::runtime_error(
            std::string("natsConnection_Subscribe failed: ") + natsStatus_GetText(s));

    auto si = std::make_unique<subscription::impl>();
    si->sub = sub;
    si->closure = std::move(cl);
    return subscription(std::move(si));
}

subscription client::queue_subscribe(std::string_view subject,
    std::string_view queue_group,
    message_handler handler) {

    auto cl = std::make_unique<sub_closure>();
    cl->handler = std::move(handler);

    natsSubscription* sub = nullptr;
    const natsStatus s = natsConnection_QueueSubscribe(
        &sub, impl_->conn,
        make_subject(subject).c_str(),
        std::string(queue_group).c_str(),
        on_msg, cl.get());

    if (s != NATS_OK)
        throw std::runtime_error(
            std::string("natsConnection_QueueSubscribe failed: ") + natsStatus_GetText(s));

    auto si = std::make_unique<subscription::impl>();
    si->sub = sub;
    si->closure = std::move(cl);
    return subscription(std::move(si));
}

void client::js_publish(std::string_view subject,
    std::span<const std::byte> data,
    std::unordered_map<std::string, std::string> headers) {

    natsMsg* msg = make_msg(make_subject(subject), data, headers);
    jsPubAck* ack = nullptr;
    // js_PublishMsg(jsPubAck**, jsCtx*, natsMsg*, jsPubOptions*, jsErrCode*)
    const natsStatus s = js_PublishMsg(&ack, impl_->js, msg, nullptr, nullptr);
    natsMsg_Destroy(msg);
    if (ack)
        jsPubAck_Destroy(ack);

    if (s != NATS_OK)
        throw std::runtime_error(
            std::string("JetStream publish failed: ") + natsStatus_GetText(s));
}

subscription client::js_subscribe(std::string_view subject,
    std::string_view durable_name,
    message_handler handler) {

    auto cl = std::make_unique<sub_closure>();
    cl->handler = std::move(handler);

    const std::string subj_str(make_subject(subject));
    const std::string durable_str(durable_name);

    jsSubOptions sub_opts;
    jsSubOptions_Init(&sub_opts);
    sub_opts.Config.Durable = durable_str.c_str();

    natsSubscription* sub = nullptr;
    // js_Subscribe(sub, js, subject, cb, closure, jsOptions*, jsSubOptions*, jsErrCode*)
    const natsStatus s = js_Subscribe(
        &sub, impl_->js, subj_str.c_str(), on_msg, cl.get(),
        nullptr, &sub_opts, nullptr);

    if (s != NATS_OK)
        throw std::runtime_error(
            std::string("js_Subscribe failed: ") + natsStatus_GetText(s));

    auto si = std::make_unique<subscription::impl>();
    si->sub = sub;
    si->closure = std::move(cl);
    return subscription(std::move(si));
}

subscription client::js_queue_subscribe(std::string_view subject,
    std::string_view durable_name,
    std::string_view queue_group,
    message_handler handler) {

    auto cl = std::make_unique<sub_closure>();
    cl->handler = std::move(handler);

    const std::string subj_str(make_subject(subject));
    const std::string durable_str(durable_name);
    const std::string queue_str(queue_group);

    jsSubOptions sub_opts;
    jsSubOptions_Init(&sub_opts);
    sub_opts.Config.Durable = durable_str.c_str();
    sub_opts.Queue = queue_str.c_str();

    natsSubscription* sub = nullptr;
    const natsStatus s = js_Subscribe(
        &sub, impl_->js, subj_str.c_str(), on_msg, cl.get(),
        nullptr, &sub_opts, nullptr);

    if (s != NATS_OK)
        throw std::runtime_error(
            std::string("js_QueueSubscribe failed: ") + natsStatus_GetText(s));

    auto si = std::make_unique<subscription::impl>();
    si->sub = sub;
    si->closure = std::move(cl);
    return subscription(std::move(si));
}

void client::drain() {
    if (impl_->conn)
        natsConnection_Drain(impl_->conn);
}

jetstream_admin client::make_admin() {
    return jetstream_admin(impl_->js);
}

}
