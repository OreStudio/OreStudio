/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/ShellMdiWindow.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.qt.headless/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QLabel>
#include <QSplitter>
#include <QTextCharFormat>
#include <rfl/json.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {

constexpr int max_shell_output_lines = 10000;

}

// --- qt_output_streambuf ---

qt_output_streambuf::qt_output_streambuf(QObject* parent)
    : QObject(parent) {}

qt_output_streambuf::int_type qt_output_streambuf::overflow(int_type ch) {
    if (ch == traits_type::eof())
        return traits_type::eof();

    std::lock_guard lock(mutex_);
    buffer_ += static_cast<char>(ch);
    if (ch == '\n')
        flush_buffer();

    return ch;
}

int qt_output_streambuf::sync() {
    std::lock_guard lock(mutex_);
    flush_buffer();
    return 0;
}

void qt_output_streambuf::flush_buffer() {
    if (buffer_.empty())
        return;

    emit text_ready(QString::fromStdString(buffer_));
    buffer_.clear();
}

// --- qt_input_streambuf ---

void qt_input_streambuf::feed_line(const std::string& line) {
    std::lock_guard lock(mutex_);
    pending_.push_back(line + "\n");
    cv_.notify_one();
}

void qt_input_streambuf::close() {
    std::lock_guard lock(mutex_);
    closed_ = true;
    cv_.notify_one();
}

qt_input_streambuf::int_type qt_input_streambuf::underflow() {
    if (gptr() < egptr())
        return traits_type::to_int_type(*gptr());

    std::unique_lock lock(mutex_);
    current_.clear();
    setg(nullptr, nullptr, nullptr);

    cv_.wait(lock, [this]() { return !pending_.empty() || closed_; });

    if (pending_.empty() && closed_)
        return traits_type::eof();

    current_ = std::move(pending_.front());
    pending_.pop_front();

    setg(current_.data(), current_.data(), current_.data() + current_.size());
    return traits_type::to_int_type(*gptr());
}

// --- ShellMdiWindow ---

ShellMdiWindow::ShellMdiWindow(ClientManager* clientManager, QWidget* parent)
    : QWidget(parent)
    , client_manager_(clientManager) {
    setup_ui();

    // Always start the REPL — even with no connection or login — so the
    // shell is usable for connecting and provisioning a fresh system.
    start_shell();
}

ShellMdiWindow::~ShellMdiWindow() {
    stop_shell();
}

void ShellMdiWindow::setup_ui() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    // A horizontal splitter: the script sidebar on the left, the
    // interactive terminal — the dominant feature — on the right.
    // (Script loading/saving lives in the sidebar, not a toolbar.)
    auto* splitter = new QSplitter(Qt::Horizontal, this);

    script_panel_ = new ScriptLibraryPanel(splitter);
    // Opening a script is owned by the main window, which spawns a
    // standalone editor MDI window; the panel itself is a pure browser.
    connect(script_panel_,
            &ScriptLibraryPanel::openRequested,
            this,
            &ShellMdiWindow::openScriptRequested);
    // Execute runs the script straight in this terminal (load <path>).
    connect(script_panel_, &ScriptLibraryPanel::executeRequested, this, &ShellMdiWindow::runScript);
    connect(
        script_panel_, &ScriptLibraryPanel::statusChanged, this, &ShellMdiWindow::statusChanged);

    auto* shell_pane = new QWidget(splitter);
    auto* shell_layout = new QVBoxLayout(shell_pane);
    shell_layout->setContentsMargins(0, 0, 0, 0);
    shell_layout->setSpacing(4);

    auto* shell_header = new QLabel("Terminal", shell_pane);
    shell_header->setStyleSheet("font-weight: bold; padding: 2px;");
    shell_layout->addWidget(shell_header);

    // Output area — monospace so table output aligns. setFont() is
    // authoritative now the QSS theme sets no font-family.
    output_area_ = new QPlainTextEdit(shell_pane);
    output_area_->setReadOnly(true);
    output_area_->setMaximumBlockCount(max_shell_output_lines);
    output_area_->setFont(FontUtils::monospace());

    // Input line, directly beneath the output as in a normal REPL.
    input_line_ = new QLineEdit(shell_pane);
    input_line_->setFont(FontUtils::monospace());
    input_line_->setPlaceholderText("Enter command...");

    shell_layout->addWidget(output_area_);
    shell_layout->addWidget(input_line_);

    splitter->addWidget(script_panel_);
    splitter->addWidget(shell_pane);
    splitter->setStretchFactor(0, 1);
    splitter->setStretchFactor(1, 3);
    layout->addWidget(splitter);

    connect(input_line_, &QLineEdit::returnPressed, this, &ShellMdiWindow::on_command_entered);

    input_line_->setFocus();
}

void ShellMdiWindow::start_shell() {
    BOOST_LOG_SEV(lg(), info) << "Starting embedded shell session.";

    // Create stream buffers
    output_buf_ = std::make_unique<qt_output_streambuf>();
    input_buf_ = std::make_unique<qt_input_streambuf>();

    // Connect output signal (cross-thread via QueuedConnection)
    connect(output_buf_.get(),
            &qt_output_streambuf::text_ready,
            this,
            &ShellMdiWindow::on_output_ready,
            Qt::QueuedConnection);

    out_stream_ = std::make_unique<std::ostream>(output_buf_.get());
    in_stream_ = std::make_unique<std::istream>(input_buf_.get());

    // Build the connection context the embedded REPL's connect command
    // inherits: the same TLS material and subject prefix the application
    // itself was configured with, both sourced from the environment so
    // they are complete before the application has connected. The shell
    // can then connect to the secured broker on its own with a plain
    // "connect <host> <port>" — which is how a fresh, bootstrap-mode
    // system is provisioned from here without the UI connecting first
    // (the UI connect triggers the provisioning wizards).
    nats::config::nats_options shell_opts;
    using ores::platform::environment::environment;
    // Subject prefix: from the environment (where it lives before the
    // app connects), falling back to the live value once it has.
    if (auto v = environment::get_value("ORES_NATS_SUBJECT_PREFIX"))
        shell_opts.subject_prefix = *v;
    if (shell_opts.subject_prefix.empty())
        shell_opts.subject_prefix = client_manager_->subjectPrefix();
    if (auto v = environment::get_value("ORES_NATS_TLS_CA"))
        shell_opts.tls_ca_cert = *v;
    if (auto v = environment::get_value("ORES_NATS_TLS_CERT"))
        shell_opts.tls_client_cert = *v;
    if (auto v = environment::get_value("ORES_NATS_TLS_KEY"))
        shell_opts.tls_client_key = *v;

    if (!client_manager_->isConnected()) {
        // Opened before the application connected. Leave the shell a
        // usable bare REPL; the context already carries TLS and the
        // subject prefix, so a plain connect (no flags) is all it takes.
        BOOST_LOG_SEV(lg(), info) << "Opening shell with no application connection.";
        output_area_->appendPlainText(
            "Not connected. Run 'connect <host> <port>' (TLS and namespace are "
            "inherited from the application), then 'bootstrap'/'login' or "
            "'provision system ...' to provision a fresh system.");
    } else {
        // The application is connected: connect the shell's own session
        // to the same broker.
        nats::config::nats_options opts = shell_opts;
        opts.url = "nats://" + client_manager_->connectedHost() + ":" +
                   std::to_string(client_manager_->connectedPort());
        bool shell_connected = false;
        try {
            shell_session_.connect(std::move(opts));
            shell_connected = true;
        } catch (const std::exception& e) {
            auto msg = QString("Shell: Failed to connect to server: %1")
                           .arg(QString::fromStdString(e.what()));
            BOOST_LOG_SEV(lg(), error) << msg.toStdString();
            output_area_->appendPlainText(msg);
        }

        // Auto-login only when the Qt session is itself logged in, and
        // only if the connect above succeeded — otherwise a login request
        // would run against a disconnected session and just emit a second
        // confusing error. On a fresh, bootstrap-mode system there is no
        // account yet, so skip the login and leave the shell usable.
        if (!shell_connected) {
            // Connect failed; the message above already explains it.
        } else if (!client_manager_->isLoggedIn()) {
            output_area_->appendPlainText(
                "Connected. Not logged in — use 'bootstrap'/'login', or 'provision "
                "system ...' to provision a fresh system.");
        } else {
            try {
                iam::messaging::login_request req{.principal = client_manager_->storedUsername(),
                                                  .password = client_manager_->storedPassword()};
                const auto json_body = rfl::json::write(req);
                auto msg =
                    shell_session_.request(iam::messaging::login_request::nats_subject, json_body);
                const std::string_view data(reinterpret_cast<const char*>(msg.data.data()),
                                            msg.data.size());
                auto resp = rfl::json::read<iam::messaging::login_response>(data);
                if (!resp || !resp->success) {
                    const std::string err = resp ? resp->error_message : "Invalid response";
                    auto qmsg = QString("Shell: Login failed: %1").arg(QString::fromStdString(err));
                    BOOST_LOG_SEV(lg(), error) << qmsg.toStdString();
                    output_area_->appendPlainText(qmsg);
                } else {
                    shell_session_.set_auth(ores::nats::service::nats_client::login_info{
                        .jwt = resp->token,
                        .username = resp->username,
                        .tenant_id = resp->tenant_id,
                        .tenant_name = resp->tenant_name});
                }
            } catch (const std::exception& e) {
                auto qmsg =
                    QString("Shell: Login failed: %1").arg(QString::fromStdString(e.what()));
                BOOST_LOG_SEV(lg(), error) << qmsg.toStdString();
                output_area_->appendPlainText(qmsg);
            }
        }
    }

    // Create REPL and run on worker thread. The REPL always runs — even
    // when not connected — so the user can drive connect/bootstrap/
    // provision themselves; it carries the connection template so its
    // connect command reuses the TLS and subject prefix above. Only the
    // TLS and subject prefix matter in the template: shell_opts.url is
    // left at its struct default, since connect always builds a fresh
    // URL from its host/port (or $ORES_NATS_URL) and overrides it.
    shell_repl_ = std::make_unique<shell::app::repl>(shell_session_, shell_opts);

    auto* in = in_stream_.get();
    auto* out = out_stream_.get();
    worker_thread_ = std::make_unique<std::thread>([this, in, out]() {
        shell_repl_->run(*in, *out);
        // Signal the UI thread that the REPL has finished
        QMetaObject::invokeMethod(this, &ShellMdiWindow::on_repl_finished, Qt::QueuedConnection);
    });
}

void ShellMdiWindow::stop_shell() {
    if (!worker_thread_)
        return;

    BOOST_LOG_SEV(lg(), info) << "Stopping embedded shell session.";

    // Send EOF to unblock the REPL's getline loop
    if (input_buf_)
        input_buf_->close();

    // Wait for the worker thread to finish
    if (worker_thread_->joinable())
        worker_thread_->join();

    worker_thread_.reset();
    shell_repl_.reset();
    if (shell_session_.is_connected())
        shell_session_.disconnect();
    in_stream_.reset();
    out_stream_.reset();
    input_buf_.reset();
    output_buf_.reset();

    BOOST_LOG_SEV(lg(), info) << "Embedded shell session stopped.";
}

void ShellMdiWindow::on_repl_finished() {
    BOOST_LOG_SEV(lg(), info) << "REPL session finished.";
    input_line_->setEnabled(false);
    input_line_->setPlaceholderText("Session ended.");
    emit statusChanged("Shell session ended.");
}

void ShellMdiWindow::on_command_entered() {
    auto text = input_line_->text();
    input_line_->clear();

    // Track command for Save
    auto cmd = text.toStdString();
    if (!cmd.empty())
        command_history_.push_back(cmd);

    // Echo the command in input color after the prompt, then newline
    QTextCharFormat fmt;
    fmt.setForeground(input_color_);
    auto cursor = output_area_->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertText(text + "\n", fmt);
    output_area_->ensureCursorVisible();

    if (input_buf_)
        input_buf_->feed_line(cmd);
}

void ShellMdiWindow::on_output_ready(const QString& text) {
    auto cursor = output_area_->textCursor();
    cursor.movePosition(QTextCursor::End);

    // Colour by status marker so success, warnings and errors are
    // instantly distinguishable, with the prompt in its own colour.
    // The shell emits ✓ / ⚠ / ✗ prefixes for these states.
    const bool is_prompt = !text.contains('\n') && text.endsWith("> ");
    const QString trimmed = text.trimmed();

    QTextCharFormat fmt;
    if (is_prompt)
        fmt.setForeground(prompt_color_);
    else if (trimmed.startsWith(QChar(0x2717))) // ✗ error
        fmt.setForeground(error_color_);
    else if (trimmed.startsWith(QChar(0x26A0))) // ⚠ warning
        fmt.setForeground(warning_color_);
    else if (trimmed.startsWith(QChar(0x2713))) // ✓ success
        fmt.setForeground(success_color_);

    cursor.insertText(text, fmt);
    output_area_->ensureCursorVisible();
}

void ShellMdiWindow::runScript(const QString& path) {
    // Feed the shell exactly what the user would type — load with the
    // script path — so the embedded REPL runs it with its own
    // stop-on-error semantics and streams output into the shell view.
    if (!input_buf_) {
        emit statusChanged("Shell session is not running.");
        return;
    }
    const auto command = "load " + path.toStdString();
    command_history_.push_back(command);

    QTextCharFormat fmt;
    fmt.setForeground(input_color_);
    auto cursor = output_area_->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertText(QString::fromStdString(command) + "\n", fmt);
    output_area_->ensureCursorVisible();

    input_buf_->feed_line(command);
}

void ShellMdiWindow::refreshScripts() {
    if (script_panel_)
        script_panel_->refresh();
}

void ShellMdiWindow::closeEvent(QCloseEvent* event) {
    stop_shell();
    event->accept();
}

}
