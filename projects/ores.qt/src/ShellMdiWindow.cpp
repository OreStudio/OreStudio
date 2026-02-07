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

#include <QLabel>
#include "ores.comms/net/client_options.hpp"
#include "ores.iam/client/auth_helpers.hpp"

namespace ores::qt {

using namespace ores::logging;

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
    buffer_ = line + "\n";
    setg(buffer_.data(), buffer_.data(), buffer_.data() + buffer_.size());
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
    buffer_.clear();
    setg(nullptr, nullptr, nullptr);

    cv_.wait(lock, [this]() { return !buffer_.empty() || closed_; });

    if (closed_ && buffer_.empty())
        return traits_type::eof();

    setg(buffer_.data(), buffer_.data(), buffer_.data() + buffer_.size());
    return traits_type::to_int_type(*gptr());
}

// --- ShellMdiWindow ---

ShellMdiWindow::ShellMdiWindow(ClientManager* clientManager, QWidget* parent)
    : QWidget(parent), client_manager_(clientManager) {
    setup_ui();

    if (client_manager_->isLoggedIn())
        start_shell();
}

ShellMdiWindow::~ShellMdiWindow() {
    stop_shell();
}

void ShellMdiWindow::setup_ui() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    output_area_ = new QPlainTextEdit(this);
    output_area_->setReadOnly(true);
    output_area_->setMaximumBlockCount(10000);
    output_area_->setStyleSheet(
        "QPlainTextEdit { font-family: monospace; font-size: 11px; }");

    input_line_ = new QLineEdit(this);
    input_line_->setStyleSheet(
        "QLineEdit { font-family: monospace; font-size: 11px; }");
    input_line_->setPlaceholderText("Enter command...");

    layout->addWidget(output_area_);
    layout->addWidget(input_line_);

    connect(input_line_, &QLineEdit::returnPressed,
        this, &ShellMdiWindow::on_command_entered);

    input_line_->setFocus();
}

void ShellMdiWindow::start_shell() {
    BOOST_LOG_SEV(lg(), info) << "Starting embedded shell session.";

    // Create stream buffers
    output_buf_ = std::make_unique<qt_output_streambuf>();
    input_buf_ = std::make_unique<qt_input_streambuf>();

    // Connect output signal (cross-thread via QueuedConnection)
    connect(output_buf_.get(), &qt_output_streambuf::text_ready,
        this, &ShellMdiWindow::on_output_ready, Qt::QueuedConnection);

    out_stream_ = std::make_unique<std::ostream>(output_buf_.get());
    in_stream_ = std::make_unique<std::istream>(input_buf_.get());

    // Create the shell's own client session and connect
    shell_session_ = std::make_unique<comms::net::client_session>();

    comms::net::client_options opts;
    opts.host = client_manager_->connectedHost();
    opts.port = client_manager_->connectedPort();
    opts.client_identifier = "ores-qt-shell";
    opts.verify_certificate = false;
    opts.heartbeat_enabled = false;

    auto connect_result = shell_session_->connect(opts);
    if (!connect_result) {
        const auto& err = connect_result.error();
        auto msg = QString("Shell: Failed to connect to server: %1")
            .arg(QString::fromStdString(err.message));
        BOOST_LOG_SEV(lg(), error) << msg.toStdString();
        output_area_->appendPlainText(msg);
        input_line_->setEnabled(false);
        shell_session_.reset();
        return;
    }

    // Login using stored credentials
    auto login_result = iam::client::login(
        *shell_session_,
        client_manager_->storedUsername(),
        client_manager_->storedPassword());

    if (!login_result.success) {
        auto msg = QString("Shell: Login failed: %1")
            .arg(QString::fromStdString(login_result.error_message));
        BOOST_LOG_SEV(lg(), error) << msg.toStdString();
        output_area_->appendPlainText(msg);
        input_line_->setEnabled(false);
        shell_session_->disconnect();
        shell_session_.reset();
        return;
    }

    // Create REPL and run on worker thread
    shell_repl_ = std::make_unique<comms::shell::app::repl>(*shell_session_);

    auto* in = in_stream_.get();
    auto* out = out_stream_.get();
    worker_thread_ = std::make_unique<std::thread>([this, in, out]() {
        shell_repl_->run(*in, *out);
        // Signal the UI thread that the REPL has finished
        QMetaObject::invokeMethod(this, &ShellMdiWindow::on_repl_finished,
            Qt::QueuedConnection);
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
    shell_session_.reset();
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

    // Echo the command after the prompt, then newline
    auto cursor = output_area_->textCursor();
    cursor.movePosition(QTextCursor::End);
    output_area_->setTextCursor(cursor);
    output_area_->insertPlainText(text + "\n");

    if (input_buf_)
        input_buf_->feed_line(text.toStdString());
}

void ShellMdiWindow::on_output_ready(const QString& text) {
    // Move cursor to end before inserting to ensure text appears at bottom
    auto cursor = output_area_->textCursor();
    cursor.movePosition(QTextCursor::End);
    output_area_->setTextCursor(cursor);
    output_area_->insertPlainText(text);
    output_area_->ensureCursorVisible();
}

void ShellMdiWindow::closeEvent(QCloseEvent* event) {
    stop_shell();
    event->accept();
}

}
