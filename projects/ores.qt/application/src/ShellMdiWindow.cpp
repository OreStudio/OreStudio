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
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QFileDialog>
#include <QLabel>
#include <QSplitter>
#include <QTextCharFormat>
#include <fstream>
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

    // Toolbar
    toolbar_ = new QToolBar(this);
    toolbar_->setIconSize(QSize(16, 16));

    auto* loadAction = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::FolderOpen, IconUtils::DefaultIconColor),
        "Load Script...");
    loadAction->setToolTip("Load and execute a .ores script file");
    connect(loadAction, &QAction::triggered, this, &ShellMdiWindow::on_load_script);

    auto* saveAction = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor), "Save Script...");
    saveAction->setToolTip("Save session commands as a .ores script file");
    connect(saveAction, &QAction::triggered, this, &ShellMdiWindow::on_save_script);

    layout->addWidget(toolbar_);

    // A vertical splitter divides the window: the script library panel
    // on top, the embedded shell (output + input) below.
    auto* splitter = new QSplitter(Qt::Vertical, this);

    script_panel_ = new ScriptLibraryPanel(splitter);
    connect(script_panel_, &ScriptLibraryPanel::runRequested, this,
            &ShellMdiWindow::on_run_script);
    connect(script_panel_, &ScriptLibraryPanel::statusChanged, this,
            &ShellMdiWindow::statusChanged);

    auto* shell_pane = new QWidget(splitter);
    auto* shell_layout = new QVBoxLayout(shell_pane);
    shell_layout->setContentsMargins(0, 0, 0, 0);
    shell_layout->setSpacing(0);

    // Output area
    output_area_ = new QPlainTextEdit(shell_pane);
    output_area_->setReadOnly(true);
    output_area_->setMaximumBlockCount(max_shell_output_lines);
    output_area_->setFont(FontUtils::monospace());

    // Input line
    input_line_ = new QLineEdit(shell_pane);
    input_line_->setFont(FontUtils::monospace());
    input_line_->setPlaceholderText("Enter command...");

    shell_layout->addWidget(output_area_);
    shell_layout->addWidget(input_line_);

    splitter->addWidget(script_panel_);
    splitter->addWidget(shell_pane);
    splitter->setStretchFactor(0, 1);
    splitter->setStretchFactor(1, 2);
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

    // Build the connection template the embedded REPL's connect command
    // reuses: TLS material from the environment, subject prefix from the
    // application. The shell can then connect (or reconnect) to the
    // secured broker on its own — which is how a fresh, bootstrap-mode
    // system is provisioned from here without the UI connecting first
    // (the UI connect triggers the provisioning wizards).
    nats::config::nats_options shell_opts;
    shell_opts.subject_prefix = client_manager_->subjectPrefix();
    using ores::platform::environment::environment;
    std::string tls_ca;
    std::string tls_cert;
    std::string tls_key;
    if (auto v = environment::get_value("ORES_NATS_TLS_CA"))
        tls_ca = *v;
    if (auto v = environment::get_value("ORES_NATS_TLS_CERT"))
        tls_cert = *v;
    if (auto v = environment::get_value("ORES_NATS_TLS_KEY"))
        tls_key = *v;
    shell_opts.tls_ca_cert = tls_ca;
    shell_opts.tls_client_cert = tls_cert;
    shell_opts.tls_client_key = tls_key;

    if (!client_manager_->isConnected()) {
        // Opened before the application connected. Leave the shell a
        // usable bare REPL and show a ready-to-paste connect line with
        // the resolved TLS paths; the user edits host/port and runs it.
        BOOST_LOG_SEV(lg(), info) << "Opening shell with no application connection.";
        output_area_->appendPlainText(
            "Not connected. To connect to the server, run (edit host/port):");
        QString hint = "  connect <host> <port>";
        if (!tls_ca.empty() || !tls_cert.empty() || !tls_key.empty())
            hint += QString(" --tls-ca %1 --tls-cert %2 --tls-key %3")
                        .arg(QString::fromStdString(tls_ca),
                             QString::fromStdString(tls_cert),
                             QString::fromStdString(tls_key));
        if (!shell_opts.subject_prefix.empty())
            hint += " --subject-prefix " +
                QString::fromStdString(shell_opts.subject_prefix);
        output_area_->appendPlainText(hint);
        output_area_->appendPlainText(
            "Then 'bootstrap'/'login', or 'provision system ...' to provision a "
            "fresh system.");
    } else {
        // The application is connected: connect the shell's own session
        // to the same broker.
        nats::config::nats_options opts = shell_opts;
        opts.url = "nats://" + client_manager_->connectedHost() + ":" +
                   std::to_string(client_manager_->connectedPort());
        try {
            shell_session_.connect(std::move(opts));
        } catch (const std::exception& e) {
            auto msg = QString("Shell: Failed to connect to server: %1")
                           .arg(QString::fromStdString(e.what()));
            BOOST_LOG_SEV(lg(), error) << msg.toStdString();
            output_area_->appendPlainText(msg);
        }

        // Auto-login only when the Qt session is itself logged in. On a
        // fresh, bootstrap-mode system there is no account yet, so skip
        // the login and leave the shell connected and usable.
        if (!client_manager_->isLoggedIn()) {
            output_area_->appendPlainText(
                "Connected. Not logged in — use 'bootstrap'/'login', or 'provision "
                "system ...' to provision a fresh system.");
        } else {
            try {
                iam::messaging::login_request req{
                    .principal = client_manager_->storedUsername(),
                    .password = client_manager_->storedPassword()};
                const auto json_body = rfl::json::write(req);
                auto msg = shell_session_.request(iam::messaging::login_request::nats_subject,
                                                  json_body);
                const std::string_view data(reinterpret_cast<const char*>(msg.data.data()),
                                            msg.data.size());
                auto resp = rfl::json::read<iam::messaging::login_response>(data);
                if (!resp || !resp->success) {
                    const std::string err = resp ? resp->error_message : "Invalid response";
                    auto qmsg =
                        QString("Shell: Login failed: %1").arg(QString::fromStdString(err));
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
    // connect command reuses the TLS and subject prefix above.
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

    // Detect prompt: short text without newlines ending with "> "
    const bool is_prompt = !text.contains('\n') && text.endsWith("> ");

    QTextCharFormat fmt;
    if (is_prompt)
        fmt.setForeground(prompt_color_);

    cursor.insertText(text, fmt);
    output_area_->ensureCursorVisible();
}

void ShellMdiWindow::on_load_script() {
    auto filename = QFileDialog::getOpenFileName(
        this, "Load Shell Script", QString(), "ORE Shell Scripts (*.ores);;All Files (*)");

    if (filename.isEmpty())
        return;

    std::ifstream file(filename.toStdString());
    if (!file.is_open()) {
        output_area_->appendPlainText(QString("Error: cannot open file: %1").arg(filename));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Loading script: " << filename.toStdString();

    std::string line;
    while (std::getline(file, line)) {
        if (line.empty())
            continue;
        if (line[0] == '#')
            continue;

        // Strip leading/trailing whitespace
        auto start = line.find_first_not_of(" \t");
        if (start == std::string::npos)
            continue;
        auto end = line.find_last_not_of(" \t\r");
        auto trimmed = line.substr(start, end - start + 1);

        command_history_.push_back(trimmed);

        if (input_buf_)
            input_buf_->feed_line(trimmed);
    }

    emit statusChanged(QString("Script loaded: %1").arg(filename));
}

void ShellMdiWindow::on_run_script(const QString& path) {
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

void ShellMdiWindow::on_save_script() {
    if (command_history_.empty()) {
        emit statusChanged("No commands to save.");
        return;
    }

    auto filename = QFileDialog::getSaveFileName(
        this, "Save Shell Script", QString(), "ORE Shell Scripts (*.ores);;All Files (*)");

    if (filename.isEmpty())
        return;

    // Ensure .ores extension
    if (!filename.endsWith(".ores"))
        filename += ".ores";

    std::ofstream file(filename.toStdString());
    if (!file.is_open()) {
        output_area_->appendPlainText(QString("Error: cannot write to file: %1").arg(filename));
        return;
    }

    file << "# ORE Studio shell script\n";
    for (const auto& cmd : command_history_)
        file << cmd << "\n";

    BOOST_LOG_SEV(lg(), info) << "Saved " << command_history_.size() << " commands to "
                              << filename.toStdString();
    emit statusChanged(
        QString("Script saved: %1 (%2 commands)").arg(filename).arg(command_history_.size()));
}

void ShellMdiWindow::closeEvent(QCloseEvent* event) {
    stop_shell();
    event->accept();
}

}
