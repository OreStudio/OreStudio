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
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/compression.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/buffered_subscription.hpp"
#include "ores.nats/service/jetstream_admin.hpp"
#include "ores.nats/service/nats_connect_error.hpp"
#include "ores.nats/service/subscription.hpp"
#include <boost/asio/any_io_executor.hpp>
#include <boost/asio/as_tuple.hpp>
#include <boost/asio/experimental/channel.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/circular_buffer.hpp>
#include <boost/system/error_code.hpp>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <memory>
#include <mutex>
#include <nats/nats.h>
#include <stdexcept>
#include <string>
#include <thread>
#include <unordered_set>
#include <vector>

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
    /**
     * @brief Whether deliveries on this subscription must be acknowledged.
     *
     * A durable subscription answers its deliveries; a core one has nothing to
     * answer and its message is destroyed by the callback, as it always was.
     */
    bool durable = false;
};

// ---------------------------------------------------------------------------
// subscription::impl
// ---------------------------------------------------------------------------

struct subscription::impl {
    natsSubscription* sub = nullptr;
    std::unique_ptr<sub_closure> closure; // must outlive sub
};

// ---------------------------------------------------------------------------
// message_buffer — thread-safe circular buffer used by buffered_subscription
// ---------------------------------------------------------------------------

struct message_buffer {
    explicit message_buffer(std::size_t capacity)
        : buf_(capacity) {}

    void push(message msg) {
        std::lock_guard lock(mu_);
        buf_.push_back(std::move(msg));
    }

    std::vector<message> snapshot() const {
        std::lock_guard lock(mu_);
        return {buf_.begin(), buf_.end()};
    }

    std::size_t size() const {
        std::lock_guard lock(mu_);
        return buf_.size();
    }

private:
    mutable std::mutex mu_;
    boost::circular_buffer<message> buf_;
};

// ---------------------------------------------------------------------------
// buffered_subscription::impl
// ---------------------------------------------------------------------------

struct buffered_subscription::impl {
    explicit impl(subscription s, std::shared_ptr<message_buffer> b)
        : sub(std::move(s))
        , buffer(std::move(b)) {}
    subscription sub;
    std::shared_ptr<message_buffer> buffer;
};

// ---------------------------------------------------------------------------
// client::impl
// ---------------------------------------------------------------------------

struct client::impl {
    config::nats_options opts;
    natsConnection* conn = nullptr;
    jsCtx* js = nullptr;
    std::atomic<bool> connected{false};
    std::mutex publish_log_mu;
    std::unordered_set<std::string> logged_publish_subjects;
};

// NATS subject logging lives here, in the common client, so a single grep
// across all service logs shows both sides of every subject: publishers log
// "NATS publish"/"NATS js-publish", listeners log "NATS subscribe" variants.
// The full (prefixed) subject is always logged, so one grep term matches both
// sides. Every one of these is a per-message notice, so all of them are debug:
// at info a caller that publishes or asks on a loop writes a line per call
// forever, and a caller whose subject is a fresh request/reply inbox cannot be
// throttled at all, because no two of its subjects are ever the same.
void client::log_publish(impl& impl, std::string_view op, const std::string& full_subject) {
    std::lock_guard lock(impl.publish_log_mu);
    if (impl.logged_publish_subjects.insert(full_subject).second)
        BOOST_LOG_SEV(lg(), debug) << op << ": " << full_subject;
    else
        BOOST_LOG_SEV(lg(), trace) << op << ": " << full_subject;
}

// ---------------------------------------------------------------------------
// Internal helpers (anonymous namespace)
// ---------------------------------------------------------------------------

namespace {

// NATS async error handler: forwards connection/subscription errors to
// Boost.Log instead of the library's default stderr print.
// NATS_CONNECTION_CLOSED is expected during orderly drain — suppress it.
void on_conn_error(natsConnection* nc,
                   natsSubscription* /*sub*/,
                   natsStatus err,
                   void* /*closure*/) {
    if (err == NATS_CONNECTION_CLOSED)
        return;
    try {
        // The live connection's last-error detail (e.g. a certificate-
        // verify reason after a reconnect attempt) beats the coarse
        // natsStatus_GetText category alone.
        char detail[256] = {0};
        if (nc != nullptr)
            natsConnection_ReadLastError(nc, detail, sizeof(detail));
        if (detail[0] != '\0')
            BOOST_LOG_SEV(lg(), warn)
                << "NATS error: " << natsStatus_GetText(err) << " (" << detail << ")";
        else
            BOOST_LOG_SEV(lg(), warn) << "NATS error: " << natsStatus_GetText(err);
    } catch (...) {
        // Boost.Log may already be torn down on a NATS internal thread.
    }
}

/**
 * @brief Reports losing the connection, and getting it back.
 *
 * Without these two lines a service that dropped its connection and
 * recovered is indistinguishable in the journal from one that never
 * dropped it. That is the question an unanswered message leaves behind,
 * and no other line in any log answers it.
 */
void on_conn_disconnected(natsConnection* /*nc*/, void* /*closure*/) {
    try {
        BOOST_LOG_SEV(lg(), warn)
            << "NATS disconnected: the server is unreachable and subscriptions are not "
               "receiving.";
    } catch (...) {
        // Boost.Log may already be torn down on a NATS internal thread.
    }
}

void on_conn_reconnected(natsConnection* /*nc*/, void* /*closure*/) {
    try {
        BOOST_LOG_SEV(lg(), warn)
            << "NATS reconnected: subscriptions are receiving again. Anything published "
               "while disconnected was delivered only if its subject is on a stream.";
    } catch (...) {
        // Boost.Log may already be torn down on a NATS internal thread.
    }
}

/**
 * @brief Answers a JetStream delivery on the cnats message it arrived on.
 *
 * The three verbs are the whole reply to a durable delivery: ack for committed
 * work, nak for work to try again, term for a message that can never be
 * handled. Only the first answer counts, so a handler that answers and then
 * unwinds does not double-answer.
 *
 * The handle owns the cnats message, so the message outlives every copy of the
 * C++ view that carries this handle and dies exactly when the last of them
 * does. A delivery without a handle -- every core NATS message -- is destroyed
 * by the callback instead, as it always was.
 *
 * Not thread-safe by itself; the transport answers each delivery once, on the
 * thread that delivered it.
 */
class ack_sink final : public ack_handle {
public:
    explicit ack_sink(natsMsg* msg) noexcept
        : msg_(msg, &natsMsg_Destroy) {}

    void accept() override {
        answer([this] { natsMsg_Ack(msg_.get(), nullptr); });
    }

    void retry() override {
        answer([this] { natsMsg_Nak(msg_.get(), nullptr); });
    }

    void retry_after(std::chrono::milliseconds delay) override {
        const auto ms = static_cast<std::int64_t>(delay.count());
        answer([this, ms] { natsMsg_NakWithDelay(msg_.get(), ms, nullptr); });
    }

    void reject() override {
        answer([this] { natsMsg_Term(msg_.get(), nullptr); });
    }

    /// @return true once the handler, or the transport's backstop, answered.
    [[nodiscard]] bool answered() const noexcept {
        return answered_;
    }

private:
    void answer(const std::function<void()>& verb) {
        if (answered_)
            return;
        answered_ = true;
        verb();
    }

    std::unique_ptr<natsMsg, decltype(&natsMsg_Destroy)> msg_;
    bool answered_ = false;
};

/**
 * @brief True when a delivery is JetStream and must be answered.
 *
 * A core NATS message has no JetStream metadata and nothing to acknowledge, so
 * the callback destroys it rather than answering it. The message type's
 * internals are private to the nats.c library, so the answer is not read off
 * the delivery but supplied by the subscription that created it: only
 * @c js_queue_subscribe makes the sink below.
 */
message extract_message(natsMsg* msg, std::shared_ptr<ack_sink> ack = {}) {
    message m;
    m.ack = std::move(ack);

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

    m.data = ores::nats::decompress_if_flagged(m.data, m.headers);
    return m;
}

// cnats message callback used by all subscribe variants.
//
// IMPORTANT: This is a C callback running on a NATS library thread.
// Exceptions must NOT propagate out — they would cross a C frame boundary
// and call std::terminate(), killing the service without any shutdown log.
void on_msg(natsConnection*, natsSubscription*, natsMsg* msg, void* ud) {
    auto* cl = static_cast<sub_closure*>(ud);
    // Read the subject directly off msg (cheap, non-throwing) so it's
    // available for the catch blocks below even if extract_message()
    // itself throws -- e.g. decompress_if_flagged() on a message that
    // claims gzip encoding but isn't valid gzip (corrupted in transit,
    // or a malformed/malicious sender on a shared bus). That call must
    // stay inside this try: an uncaught exception here crosses a C frame
    // boundary into std::terminate(), killing the whole process with no
    // log line for what would otherwise be a single droppable bad message.
    const char* s = natsMsg_GetSubject(msg);
    const std::string subject = s ? s : std::string();
    // A durable delivery is answered by the handler, through this handle; the
    // transport answers it only if the handler left it undecided. The handle
    // owns the cnats message, so a delivery that carries one is freed when the
    // last copy of the handler's message is. Held outside the try so the catch
    // blocks can answer a delivery whose handler threw.
    auto ack = cl->durable ? std::make_shared<ack_sink>(msg) : nullptr;
    try {
        cl->handler(extract_message(msg, ack));
    } catch (const std::exception& e) {
        if (!ack && msg)
            natsMsg_Destroy(msg);
        msg = nullptr;
        BOOST_LOG_SEV(lg(), error) << "Unhandled exception in NATS message handler for subject '"
                                   << subject << "': " << e.what();
    } catch (...) {
        if (!ack && msg)
            natsMsg_Destroy(msg);
        msg = nullptr;
        BOOST_LOG_SEV(lg(), error)
            << "Unknown exception in NATS message handler for subject '" << subject << "'";
    }
    // The backstop. A handler that answered the delivery itself has nothing
    // left to do; one that returned without deciding, or threw before
    // deciding, is asked for a retry rather than left acknowledged.
    if (ack && !ack->answered())
        ack->retry();
}

// Build a natsMsg* with data and optional headers.
// Caller owns the result and must call natsMsg_Destroy.
natsMsg* make_msg(std::string_view subject,
                  std::span<const std::byte> data,
                  std::unordered_map<std::string, std::string> headers,
                  const char* reply = nullptr) {

    const auto payload = ores::nats::compress_if_worthwhile(data, headers);

    natsMsg* msg = nullptr;
    const natsStatus s = natsMsg_Create(&msg,
                                        std::string(subject).c_str(),
                                        reply,
                                        reinterpret_cast<const char*>(payload.data()),
                                        static_cast<int>(payload.size()));

    if (s != NATS_OK)
        throw std::runtime_error(std::string("natsMsg_Create failed: ") + natsStatus_GetText(s));

    for (const auto& [k, v] : headers)
        natsMsgHeader_Set(msg, k.c_str(), v.c_str());

    return msg;
}

} // namespace

// ---------------------------------------------------------------------------
// subscription — methods defined here because subscription::impl lives here
// ---------------------------------------------------------------------------

subscription::subscription(std::unique_ptr<impl> i)
    : impl_(std::move(i)) {}

// ---------------------------------------------------------------------------
// Async drain helpers — shared by ~subscription and operator=(subscription&&)
// ---------------------------------------------------------------------------

// Heap-allocated context that keeps sub_closure alive until the NATS dispatch
// thread has finished all in-flight on_msg calls for this subscription.
struct sub_drain_ctx {
    natsSubscription* sub;
    std::unique_ptr<sub_closure> closure;
};

// C-linkage callback registered with natsSubscription_SetOnCompleteCB.
// Fires from the dispatch thread after drain: all on_msg calls have returned,
// so it is now safe to free the closure and destroy the NATS subscription.
extern "C" void drain_cb(void* ud) noexcept {
    auto* c = static_cast<sub_drain_ctx*>(ud);
    c->closure.reset();
    natsSubscription_Destroy(c->sub);
    delete c;
}

// Initiates an async drain of impl_->sub and transfers ownership of both the
// sub and closure to a heap context freed by drain_cb.  After this call,
// impl_->sub is nullptr and impl_->closure is empty.
static void drain_impl(subscription::impl* impl) noexcept {
    if (!impl || !impl->sub)
        return;
    auto* ctx = new (std::nothrow) sub_drain_ctx{impl->sub, std::move(impl->closure)};
    if (ctx) {
        natsSubscription_SetOnCompleteCB(impl->sub, drain_cb, ctx);
        natsSubscription_Drain(impl->sub);
    }
    // On alloc failure: closure leaks (no crash), sub leaks (no crash).
    impl->sub = nullptr;
}

subscription::~subscription() {
    drain_impl(impl_.get());
}


subscription::subscription(subscription&&) noexcept = default;

subscription& subscription::operator=(subscription&& other) noexcept {
    if (this == &other)
        return *this;
    drain_impl(impl_.get()); // drain old before taking ownership of new
    impl_ = std::move(other.impl_);
    return *this;
}

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
// buffered_subscription — methods defined here because impl lives here
// ---------------------------------------------------------------------------

buffered_subscription::buffered_subscription(std::unique_ptr<impl> i)
    : impl_(std::move(i)) {}

buffered_subscription::~buffered_subscription() = default;
buffered_subscription::buffered_subscription(buffered_subscription&&) noexcept = default;
buffered_subscription& buffered_subscription::operator=(buffered_subscription&&) noexcept = default;

std::vector<message> buffered_subscription::snapshot() const {
    if (!impl_)
        return {};
    return impl_->buffer->snapshot();
}

std::size_t buffered_subscription::size() const {
    if (!impl_)
        return 0;
    return impl_->buffer->size();
}

// ---------------------------------------------------------------------------
// client
// ---------------------------------------------------------------------------

client::client(config::nats_options opts)
    : impl_(std::make_unique<impl>()) {
    impl_->opts = std::move(opts);
    set_default_wire_codec(wire_codec(impl_->opts.format));
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
    natsOptions_SetDisconnectedCB(opts, on_conn_disconnected, nullptr);
    natsOptions_SetReconnectedCB(opts, on_conn_reconnected, nullptr);

    if (!impl_->opts.tls_ca_cert.empty()) {
        // Validate TLS files exist before attempting connection so we can log
        // a clear diagnostic instead of a cryptic SSL handshake error.
        auto check_file = [](const std::string& path, const char* role) {
            if (!std::filesystem::exists(path))
                throw std::runtime_error(std::string("NATS TLS ") + role + " not found: " + path);
        };
        check_file(impl_->opts.tls_ca_cert, "CA certificate");
        check_file(impl_->opts.tls_client_cert, "client certificate");
        check_file(impl_->opts.tls_client_key, "client key");

        natsOptions_SetSecure(opts, true);
        const natsStatus ca_status =
            natsOptions_LoadCATrustedCertificates(opts, impl_->opts.tls_ca_cert.c_str());
        if (ca_status != NATS_OK) {
            natsOptions_Destroy(opts);
            throw nats_connect_error(nats_error_kind::tls_error,
                                     std::string("Failed to load NATS CA certificate (") +
                                         impl_->opts.tls_ca_cert +
                                         "): " + natsStatus_GetText(ca_status) +
                                         " -- check the file is actually readable by this "
                                         "process's real uid (rootless podman without "
                                         "--userns=keep-id remaps uids, so a mode-600 file "
                                         "owned by the host user may be unreadable even "
                                         "though it exists)");
        }
        const natsStatus chain_status = natsOptions_LoadCertificatesChain(
            opts, impl_->opts.tls_client_cert.c_str(), impl_->opts.tls_client_key.c_str());
        if (chain_status != NATS_OK) {
            natsOptions_Destroy(opts);
            throw nats_connect_error(nats_error_kind::tls_error,
                                     std::string("Failed to load NATS client certificate/key (") +
                                         impl_->opts.tls_client_cert + ", " +
                                         impl_->opts.tls_client_key +
                                         "): " + natsStatus_GetText(chain_status) +
                                         " -- check the files are actually readable by this "
                                         "process's real uid (rootless podman without "
                                         "--userns=keep-id remaps uids, so a mode-600 file "
                                         "owned by the host user may be unreadable even "
                                         "though it exists)");
        }
        BOOST_LOG_SEV(lg(), info) << "mTLS enabled (ca=" << impl_->opts.tls_ca_cert
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
        // natsStatus_GetText only yields the coarse category ("SSL Error").
        // The thread-local last error carries nats.c's fine-grained detail
        // (e.g. the OpenSSL certificate-verify reason with the peer cert
        // subject), which is what makes a failed handshake diagnosable.
        std::string detail;
        if (const char* last = nats_GetLastError(nullptr))
            detail = last;
        throw nats_connect_error(
            kind,
            std::string("NATS connect failed: ") + natsStatus_GetText(s) +
                (detail.empty() ? std::string() : std::string(" (") + detail + ")"));
    }

    // Note: natsConnection_JetStream(jsCtx**, natsConnection*, jsOptions*)
    s = natsConnection_JetStream(&impl_->js, impl_->conn, nullptr);
    if (s != NATS_OK) {
        natsConnection_Close(impl_->conn);
        natsConnection_Destroy(impl_->conn);
        impl_->conn = nullptr;
        throw std::runtime_error(std::string("NATS JetStream init failed: ") +
                                 natsStatus_GetText(s));
    }

    impl_->connected.store(true, std::memory_order_release);
    BOOST_LOG_SEV(lg(), info) << "NATS connect: " << impl_->opts.url << " (subject prefix: '"
                              << impl_->opts.subject_prefix << "')";
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

std::string client::subjects_below(std::string_view subject) const {
    return make_subject(subject) + ".>";
}

std::vector<std::string>
client::covering_subjects(std::initializer_list<std::string_view> subjects) const {
    std::vector<std::string> filters;
    filters.reserve(subjects.size() * 2);
    for (const auto subject : subjects) {
        filters.push_back(make_subject(subject));
        filters.push_back(subjects_below(subject));
    }
    return filters;
}

void client::publish(std::string_view subject,
                     std::span<const std::byte> data,
                     std::unordered_map<std::string, std::string> headers) {

    const std::string full_subject(make_subject(subject));
    log_publish(*impl_, "NATS publish", full_subject);

    natsMsg* msg = make_msg(full_subject, data, headers);
    const natsStatus s = natsConnection_PublishMsg(impl_->conn, msg);
    natsMsg_Destroy(msg);
    if (s != NATS_OK)
        throw std::runtime_error(std::string("NATS publish failed: ") + natsStatus_GetText(s));
}

message client::request_sync(std::string_view subject,
                             std::span<const std::byte> data,
                             std::unordered_map<std::string, std::string> headers,
                             std::chrono::milliseconds timeout) {

    const std::string full_subject(make_subject(subject));
    BOOST_LOG_SEV(lg(), debug) << "NATS request: " << full_subject;

    natsMsg* req = make_msg(full_subject, data, headers);
    natsMsg* reply = nullptr;
    const natsStatus s =
        natsConnection_RequestMsg(&reply, impl_->conn, req, static_cast<int64_t>(timeout.count()));
    natsMsg_Destroy(req);

    if (s != NATS_OK) {
        if (s == NATS_TIMEOUT)
            throw std::runtime_error("NATS request timed out");
        if (s == NATS_NO_RESPONDERS)
            throw nats_connect_error(nats_error_kind::services_unavailable,
                                     "NATS request failed: no service is handling subject '" +
                                         std::string(make_subject(subject)) + "'");
        throw std::runtime_error(std::string("NATS request failed: ") + natsStatus_GetText(s));
    }

    message m = extract_message(reply);
    natsMsg_Destroy(reply);
    return m;
}

boost::asio::awaitable<message>
client::request(std::string_view subject,
                std::span<const std::byte> data,
                std::unordered_map<std::string, std::string> headers,
                std::chrono::milliseconds timeout) {

    using chan_t = boost::asio::experimental::channel<boost::asio::any_io_executor,
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
            chan->try_send(boost::system::error_code{boost::asio::error::operation_aborted},
                           message{});
        }
    }).detach();

    auto [ec, msg] =
        co_await chan->async_receive(boost::asio::as_tuple(boost::asio::use_awaitable));

    if (ec)
        throw std::runtime_error("NATS async request failed: " + ec.message());

    co_return std::move(msg);
}

subscription client::subscribe(std::string_view subject, message_handler handler) {
    auto cl = std::make_unique<sub_closure>();
    cl->handler = std::move(handler);

    const std::string full_subject(make_subject(subject));
    BOOST_LOG_SEV(lg(), info) << "NATS subscribe: " << full_subject;

    natsSubscription* sub = nullptr;
    const natsStatus s =
        natsConnection_Subscribe(&sub, impl_->conn, full_subject.c_str(), on_msg, cl.get());

    if (s != NATS_OK)
        throw std::runtime_error(std::string("natsConnection_Subscribe failed: ") +
                                 natsStatus_GetText(s));

    auto si = std::make_unique<subscription::impl>();
    si->sub = sub;
    si->closure = std::move(cl);
    return subscription(std::move(si));
}

buffered_subscription client::subscribe_buffered(std::string_view subject,
                                                 std::size_t capacity,
                                                 message_handler handler) {
    auto buf = std::make_shared<message_buffer>(capacity);

    // Wrap the caller's handler: push a copy to the buffer, then forward.
    auto sub = subscribe(subject, [buf, h = std::move(handler)](message msg) mutable {
        buf->push(msg); // copy before the potential move into h
        if (h)
            h(std::move(msg));
    });

    auto bi = std::make_unique<buffered_subscription::impl>(std::move(sub), std::move(buf));
    return buffered_subscription(std::move(bi));
}

subscription client::queue_subscribe(std::string_view subject,
                                     std::string_view queue_group,
                                     message_handler handler) {

    auto cl = std::make_unique<sub_closure>();
    cl->handler = std::move(handler);

    const std::string full_subject(make_subject(subject));
    BOOST_LOG_SEV(lg(), info) << "NATS queue-subscribe: " << full_subject
                              << " (group: " << queue_group << ")";

    natsSubscription* sub = nullptr;
    const natsStatus s = natsConnection_QueueSubscribe(&sub,
                                                       impl_->conn,
                                                       full_subject.c_str(),
                                                       std::string(queue_group).c_str(),
                                                       on_msg,
                                                       cl.get());

    if (s != NATS_OK)
        throw std::runtime_error(std::string("natsConnection_QueueSubscribe failed: ") +
                                 natsStatus_GetText(s));

    auto si = std::make_unique<subscription::impl>();
    si->sub = sub;
    si->closure = std::move(cl);
    return subscription(std::move(si));
}

void client::js_publish(std::string_view subject,
                        std::span<const std::byte> data,
                        std::unordered_map<std::string, std::string> headers) {

    const std::string full_subject(make_subject(subject));
    log_publish(*impl_, "NATS js-publish", full_subject);

    natsMsg* msg = make_msg(full_subject, data, headers);
    jsPubAck* ack = nullptr;
    // js_PublishMsg(jsPubAck**, jsCtx*, natsMsg*, jsPubOptions*, jsErrCode*)
    const natsStatus s = js_PublishMsg(&ack, impl_->js, msg, nullptr, nullptr);
    natsMsg_Destroy(msg);
    if (ack)
        jsPubAck_Destroy(ack);

    if (s != NATS_OK)
        throw std::runtime_error(std::string("JetStream publish failed: ") + natsStatus_GetText(s));
}

namespace {

/**
 * @brief The filters a stored consumer holds, as a sorted list.
 *
 * The server stores one filter as @c FilterSubject and several as
 * @c FilterSubjects, so both spellings are read as the same set.
 */
std::vector<std::string> stored_filters(const jsConsumerConfig& config) {
    std::vector<std::string> stored;
    if (config.FilterSubjects != nullptr && config.FilterSubjectsLen > 0) {
        for (int i = 0; i < config.FilterSubjectsLen; ++i) {
            const char* filter = config.FilterSubjects[i];
            stored.emplace_back(filter != nullptr ? filter : "");
        }
    } else if (config.FilterSubject != nullptr) {
        stored.emplace_back(config.FilterSubject);
    }
    std::sort(stored.begin(), stored.end());
    return stored;
}

/**
 * @brief Set the filter fields of a consumer configuration.
 *
 * The server stores one filter as @c FilterSubject and several as
 * @c FilterSubjects. The client rejects a single @c FilterSubjects entry once
 * the server has normalised it, so one filter is always sent in the singular
 * form.
 *
 * @param raw Holds the pointer array alive for the call that reads @p config.
 */
void set_filters(jsConsumerConfig& config,
                 const std::vector<std::string>& filters,
                 std::vector<const char*>& raw) {
    raw.clear();
    raw.reserve(filters.size());
    for (const auto& filter : filters)
        raw.push_back(filter.c_str());

    if (raw.size() == 1) {
        config.FilterSubject = raw[0];
        config.FilterSubjects = nullptr;
        config.FilterSubjectsLen = 0;
        return;
    }
    config.FilterSubject = nullptr;
    config.FilterSubjects = raw.data();
    config.FilterSubjectsLen = static_cast<int>(raw.size());
}

/**
 * @brief Create the durable push consumer, or bring a stored one onto the
 *        configuration asked for.
 *
 * The consumer is created here, with @c js_AddConsumer, rather than by the
 * subscribe. A consumer created by @c js_Subscribe is removed by the server
 * when its subscription drains, even when it is marked durable, and every
 * service drains on shutdown; the consumer would then be gone while the
 * service restarts, and under interest retention a command published in that
 * window is dropped. Creating it explicitly and binding to it keeps the
 * consumer, and the position it holds, on the server across the restart.
 *
 * @c durable is the whole identity: the same name on every start attaches the
 * new subscription to the stored position. @c deliver_subject is stable for
 * the same reason, so the server publishes a held command to the subject the
 * next start binds. @c deliver_group is the queue group, so replicas share the
 * one consumer exactly as they shared the queue subscription before.
 *
 * Idempotent. An existing consumer whose configuration already matches is
 * reused as it stands, which is the normal restart path. One whose filter set,
 * ack policy, delivery bound, ack wait, delivery subject or delivery group
 * differs is updated in place, so the consumer keeps its position: a subject
 * rename or a widened filter set does not replay a backlog it had already
 * worked through. The stored configuration is the base of the update, so every
 * field this code does not set keeps the value the server holds.
 *
 * A message the stored consumer had already given up on is not pending, so the
 * update does not bring it back. Raising the bound changes what future
 * deliveries may do; it does not revive the work already discarded.
 *
 * A consumer that cannot be created or reconciled throws: the service would
 * otherwise run with a filter or a position it did not ask for, and silently
 * miss commands.
 */
void ensure_durable_consumer(jsCtx* js,
                             const std::string& stream,
                             const std::string& durable,
                             const std::string& deliver_subject,
                             const std::string& deliver_group,
                             const std::vector<std::string>& filters,
                             int max_deliver,
                             std::chrono::milliseconds ack_wait) {
    jsOptions opts;
    jsOptions_Init(&opts);

    auto read_consumer = [&]() -> jsConsumerInfo* {
        jsConsumerInfo* info = nullptr;
        auto jerr = jsErrCode(0);
        const natsStatus s =
            js_GetConsumerInfo(&info, js, stream.c_str(), durable.c_str(), &opts, &jerr);
        if (s == NATS_NOT_FOUND)
            return nullptr;
        // Any other failure is not "the consumer is absent": a permission or
        // connection problem read as absent would silently try to create a
        // consumer that already exists, and report the create's refusal.
        if (s != NATS_OK) {
            if (info != nullptr)
                jsConsumerInfo_Destroy(info);
            throw std::runtime_error("js_GetConsumerInfo failed for '" + durable +
                                     "': " + natsStatus_GetText(s));
        }
        return info;
    };

    // Named `existing`, not `info`: `info` is a severity enumerator in this
    // logging facade, and a local of that name shadows it inside BOOST_LOG_SEV.
    jsConsumerInfo* existing = read_consumer();

    if (existing == nullptr) {
        jsConsumerConfig config;
        jsConsumerConfig_Init(&config);
        config.Durable = durable.c_str();
        config.DeliverSubject = deliver_subject.c_str();
        config.DeliverGroup = deliver_group.empty() ? nullptr : deliver_group.c_str();
        config.AckPolicy = js_AckExplicit;
        config.DeliverPolicy = js_DeliverAll;
        config.ReplayPolicy = js_ReplayInstant;
        config.MaxDeliver = max_deliver;
        config.AckWait = ack_wait.count() * 1'000'000LL;

        // The add reads these pointers during the call, so they outlive it here.
        std::vector<const char*> raw;
        set_filters(config, filters, raw);

        jsConsumerInfo* created = nullptr;
        auto jerr = jsErrCode(0);
        const natsStatus cs = js_AddConsumer(&created, js, stream.c_str(), &config, &opts, &jerr);
        if (created != nullptr)
            jsConsumerInfo_Destroy(created);
        if (cs == NATS_OK) {
            BOOST_LOG_SEV(lg(), info) << "NATS durable '" << durable << "' on '" << stream
                                      << "' created for delivery to '" << deliver_subject << "'";
            return;
        }

        // Another replica may have created it between the read and the add, so
        // read again and reconcile rather than fail the start on that race.
        existing = read_consumer();
        if (existing == nullptr)
            throw std::runtime_error("NATS durable '" + durable + "' on '" + stream +
                                     "' could not be created: " + natsStatus_GetText(cs));
    }

    jsConsumerConfig* config = existing->Config;
    if (config == nullptr) {
        jsConsumerInfo_Destroy(existing);
        throw std::runtime_error("NATS durable '" + durable + "' on '" + stream +
                                 "' has no configuration");
    }

    // All copied before the destroy: these fields are owned by the consumer
    // info, so the log line below would otherwise read freed memory.
    std::vector<std::string> stored(stored_filters(*config));
    const jsAckPolicy stored_ack = config->AckPolicy;
    const int64_t stored_bound = config->MaxDeliver;
    const int64_t stored_ack_wait = config->AckWait;
    const std::string stored_deliver(config->DeliverSubject != nullptr ? config->DeliverSubject :
                                                                         "");
    const std::string stored_group(config->DeliverGroup != nullptr ? config->DeliverGroup : "");

    std::vector<std::string> want(filters);
    std::sort(want.begin(), want.end());
    const int64_t want_ack_wait = ack_wait.count() * 1'000'000LL;
    const bool stale = stored != want || stored_ack != js_AckExplicit ||
                       stored_bound != static_cast<int64_t>(max_deliver) ||
                       stored_ack_wait != want_ack_wait || stored_deliver != deliver_subject ||
                       stored_group != deliver_group;
    if (!stale) {
        jsConsumerInfo_Destroy(existing);
        return;
    }

    // The update reads these pointers during the call, so they outlive it here.
    // The stored configuration is copied rather than edited: its string fields
    // are owned by `existing`, and overwriting them would leave the destroy
    // below freeing pointers this function had put there.
    jsConsumerConfig updated_config = *config;
    std::vector<const char*> raw;
    set_filters(updated_config, filters, raw);

    updated_config.DeliverSubject = deliver_subject.c_str();
    updated_config.DeliverGroup = deliver_group.empty() ? nullptr : deliver_group.c_str();
    updated_config.AckPolicy = js_AckExplicit;
    updated_config.MaxDeliver = max_deliver;
    updated_config.AckWait = want_ack_wait;

    jsConsumerInfo* updated = nullptr;
    auto jerr = jsErrCode(0);
    const natsStatus us =
        js_UpdateConsumer(&updated, js, stream.c_str(), &updated_config, &opts, &jerr);
    if (updated != nullptr)
        jsConsumerInfo_Destroy(updated);
    jsConsumerInfo_Destroy(existing);

    if (us != NATS_OK)
        throw std::runtime_error("NATS durable '" + durable + "' on '" + stream +
                                 "' holds a different configuration and could not be updated: " +
                                 natsStatus_GetText(us));

    BOOST_LOG_SEV(lg(), info) << "NATS durable '" << durable << "' on '" << stream
                              << "' migrated in place from " << stored.size() << " filter(s), ack "
                              << static_cast<int>(stored_ack) << ", max-deliver " << stored_bound
                              << ", ack-wait " << stored_ack_wait << "ns, delivery to '"
                              << stored_deliver << "' to " << filters.size()
                              << " filter(s), max-deliver " << max_deliver << ", ack-wait "
                              << want_ack_wait << "ns, delivery to '" << deliver_subject << "'";
}

}

subscription client::js_queue_subscribe(std::string_view subject,
                                        std::string_view durable_name,
                                        std::string_view queue_group,
                                        message_handler handler,
                                        std::string_view stream_name,
                                        int max_deliver,
                                        std::chrono::milliseconds ack_wait) {

    auto cl = std::make_unique<sub_closure>();
    cl->handler = std::move(handler);
    cl->durable = true;

    if (stream_name.empty())
        throw std::invalid_argument("js_queue_subscribe needs the name of the stream that holds "
                                    "the subject, because the durable consumer is created on it "
                                    "and has to outlive the subscription.");

    const std::string subj_str(make_subject(subject));
    const std::string durable_str(durable_name);
    const std::string queue_str(queue_group);
    const std::string stream_str(stream_name);
    const std::string deliver_str(
        make_subject(std::string("_deliver.") + stream_str + "." + durable_str));

    BOOST_LOG_SEV(lg(), info) << "NATS js-queue-subscribe: " << subj_str
                              << " (durable: " << durable_str << ", group: " << queue_str
                              << ", max-deliver: " << max_deliver
                              << ", ack-wait: " << ack_wait.count() << "ms)";

    ensure_durable_consumer(impl_->js,
                            stream_str,
                            durable_str,
                            deliver_str,
                            queue_str,
                            {subj_str},
                            max_deliver,
                            ack_wait);

    jsSubOptions sub_opts;
    jsSubOptions_Init(&sub_opts);
    sub_opts.Stream = stream_str.c_str();
    sub_opts.Consumer = durable_str.c_str();
    sub_opts.Config.Durable = durable_str.c_str();
    sub_opts.Config.AckPolicy = js_AckExplicit;
    sub_opts.Config.MaxDeliver = max_deliver;
    sub_opts.Config.AckWait = ack_wait.count() * 1'000'000LL;
    sub_opts.Queue = queue_str.c_str();

    natsSubscription* sub = nullptr;
    const natsStatus s = js_Subscribe(
        &sub, impl_->js, subj_str.c_str(), on_msg, cl.get(), nullptr, &sub_opts, nullptr);

    if (s != NATS_OK)
        throw std::runtime_error(std::string("js_QueueSubscribe failed: ") + natsStatus_GetText(s));

    auto si = std::make_unique<subscription::impl>();
    si->sub = sub;
    si->closure = std::move(cl);
    return subscription(std::move(si));
}

subscription client::js_queue_subscribe_multi(std::span<const std::string_view> subjects,
                                              std::string_view durable_name,
                                              std::string_view queue_group,
                                              message_handler handler,
                                              std::string_view stream_name,
                                              int max_deliver,
                                              std::chrono::milliseconds ack_wait) {

    auto cl = std::make_unique<sub_closure>();
    cl->handler = std::move(handler);
    cl->durable = true;

    if (stream_name.empty())
        throw std::invalid_argument("js_queue_subscribe_multi needs the name of the stream that "
                                    "holds the subjects, because the durable consumer is created "
                                    "on it and has to outlive the subscription.");

    std::vector<std::string> full;
    full.reserve(subjects.size());
    for (const auto subject : subjects)
        full.push_back(make_subject(subject));

    const std::string durable_str(durable_name);
    const std::string queue_str(queue_group);
    const std::string stream_str(stream_name);
    const std::string deliver_str(
        make_subject(std::string("_deliver.") + stream_str + "." + durable_str));

    BOOST_LOG_SEV(lg(), info) << "NATS js-queue-subscribe-multi: " << full.size()
                              << " subject(s) (durable: " << durable_str << ", group: " << queue_str
                              << ", max-deliver: " << max_deliver
                              << ", ack-wait: " << ack_wait.count() << "ms)";

    ensure_durable_consumer(
        impl_->js, stream_str, durable_str, deliver_str, queue_str, full, max_deliver, ack_wait);

    // The C API wants a null-terminated-style array of pointers that outlives
    // the call; the strings themselves live in `full`.
    std::vector<const char*> raw;
    raw.reserve(full.size());
    for (const auto& s : full)
        raw.push_back(s.c_str());

    jsSubOptions sub_opts;
    jsSubOptions_Init(&sub_opts);
    sub_opts.Stream = stream_str.c_str();
    sub_opts.Consumer = durable_str.c_str();
    sub_opts.Config.Durable = durable_str.c_str();
    sub_opts.Config.AckPolicy = js_AckExplicit;
    sub_opts.Config.MaxDeliver = max_deliver;
    sub_opts.Config.AckWait = ack_wait.count() * 1'000'000LL;
    sub_opts.Queue = queue_str.c_str();

    natsSubscription* sub = nullptr;
    const natsStatus s = js_SubscribeMulti(&sub,
                                           impl_->js,
                                           raw.data(),
                                           static_cast<int>(raw.size()),
                                           on_msg,
                                           cl.get(),
                                           nullptr,
                                           &sub_opts,
                                           nullptr);

    if (s != NATS_OK)
        throw std::runtime_error(std::string("js_SubscribeMulti failed: ") + natsStatus_GetText(s));

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
