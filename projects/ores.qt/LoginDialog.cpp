/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/LoginDialog.hpp"
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.comms/protocol/frame.hpp"
#include "ores.comms/protocol/message_types.hpp"
#include "ores.accounts/messaging/protocol.hpp"

namespace ores::qt {

login_dialog::login_dialog(QWidget* parent)
    : QDialog(parent),
      username_edit_(new QLineEdit(this)),
      password_edit_(new QLineEdit(this)),
      host_edit_(new QLineEdit(this)),
      port_spinbox_(new QSpinBox(this)),
      login_button_(new QPushButton("Login", this)),
      cancel_button_(new QPushButton("Cancel", this)),
      status_label_(new QLabel(this)) {

    setup_ui();

    // Connect signals
    connect(login_button_, &QPushButton::clicked, this, &login_dialog::on_login_clicked);
    connect(cancel_button_, &QPushButton::clicked, this, &QDialog::reject);
    connect(this, &login_dialog::connection_completed,
            this, &login_dialog::on_connection_result);
    connect(this, &login_dialog::login_completed,
            this, &login_dialog::on_login_result);
}

login_dialog::~login_dialog() {
    // Reset work guard to allow IO context to finish
    work_guard_.reset();

    if (io_thread_ && io_thread_->joinable()) {
        if (io_context_) {
            io_context_->stop();
        }
        io_thread_->join();
    }
}

void login_dialog::setup_ui() {
    setWindowTitle("Login to ORE Studio");
    setModal(true);
    setMinimumWidth(400);

    // Create form layout
    auto* form_layout = new QFormLayout();

    // Username field
    username_edit_->setPlaceholderText("Enter username");
    form_layout->addRow("Username:", username_edit_);

    // Password field
    password_edit_->setEchoMode(QLineEdit::Password);
    password_edit_->setPlaceholderText("Enter password");
    form_layout->addRow("Password:", password_edit_);

    // Server host field
    host_edit_->setText("localhost");
    host_edit_->setPlaceholderText("Server hostname or IP");
    form_layout->addRow("Server Host:", host_edit_);

    // Server port field
    port_spinbox_->setRange(1, 65535);
    port_spinbox_->setValue(55555);
    form_layout->addRow("Server Port:", port_spinbox_);

    // Status label
    status_label_->setWordWrap(true);
    status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Button layout
    auto* button_layout = new QHBoxLayout();
    button_layout->addStretch();
    button_layout->addWidget(cancel_button_);
    button_layout->addWidget(login_button_);

    // Main layout
    auto* main_layout = new QVBoxLayout(this);
    main_layout->addLayout(form_layout);
    main_layout->addWidget(status_label_);
    main_layout->addSpacing(10);
    main_layout->addLayout(button_layout);

    // Set default button
    login_button_->setDefault(true);
}

void login_dialog::enable_form(bool enabled) {
    username_edit_->setEnabled(enabled);
    password_edit_->setEnabled(enabled);
    host_edit_->setEnabled(enabled);
    port_spinbox_->setEnabled(enabled);
    login_button_->setEnabled(enabled);
}

void login_dialog::on_login_clicked() {
    const auto username = username_edit_->text().trimmed();
    const auto password = password_edit_->text();
    const auto host = host_edit_->text().trimmed();
    const auto port = static_cast<std::uint16_t>(port_spinbox_->value());

    // Validate input
    if (username.isEmpty()) {
        QMessageBox::warning(this, "Invalid Input", "Please enter a username.");
        username_edit_->setFocus();
        return;
    }

    if (password.isEmpty()) {
        QMessageBox::warning(this, "Invalid Input", "Please enter a password.");
        password_edit_->setFocus();
        return;
    }

    if (host.isEmpty()) {
        QMessageBox::warning(this, "Invalid Input", "Please enter a server host.");
        host_edit_->setFocus();
        return;
    }

    // Disable form during connection
    enable_form(false);
    status_label_->setText("Connecting to server...");

    // Create IO context and work guard to keep it alive
    io_context_ = std::make_unique<boost::asio::io_context>();
    work_guard_ = std::make_unique<boost::asio::executor_work_guard<boost::asio::io_context::executor_type>>(
        boost::asio::make_work_guard(*io_context_)
    );

    comms::client_options config{
        .host = host.toStdString(),
        .port = port,
        .client_identifier = "ores-qt-client",
        .verify_certificate = false
    };

    client_ = std::make_shared<comms::client>(config, io_context_->get_executor());

    // Start IO thread
    io_thread_ = std::make_unique<std::thread>([this]() {
        io_context_->run();
    });

    // Perform connection asynchronously using QtConcurrent
    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, [this, watcher, username, password]() {
        const bool connected = watcher->result();
        watcher->deleteLater();

        if (connected) {
            emit connection_completed(true, QString());
            // Proceed to login
            perform_login(username.toStdString(), password.toStdString());
        } else {
            emit connection_completed(false, "Failed to connect to server");
        }
    });

    QFuture<bool> future = QtConcurrent::run([this]() {
        return client_->connect_sync();
    });

    watcher->setFuture(future);
}

void login_dialog::perform_login(const std::string& username, const std::string& password) {
    status_label_->setText("Authenticating...");

    // Perform login asynchronously
    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished,
            [this, watcher]() {
        const auto [success, error_msg] = watcher->result();
        watcher->deleteLater();
        emit login_completed(success, error_msg);
    });

    QFuture<std::pair<bool, QString>> future = QtConcurrent::run(
        [this, username, password]() -> std::pair<bool, QString> {
            // Create login request
            accounts::messaging::login_request request{
                .username = username,
                .password = password
            };

            auto payload = request.serialize();
            comms::protocol::frame request_frame(
                comms::protocol::message_type::login_request,
                0,
                std::move(payload)
            );

            // Send request
            auto response_result = client_->send_request_sync(std::move(request_frame));

            if (!response_result) {
                return {false, QString("Network error: failed to send login request")};
            }

            // Deserialize response
            auto response = accounts::messaging::login_response::deserialize(
                response_result->payload()
            );

            if (!response) {
                return {false, QString("Protocol error: failed to parse login response")};
            }

            if (!response->success) {
                return {false, QString::fromStdString(response->error_message)};
            }

            return {true, QString()};
        }
    );

    watcher->setFuture(future);
}

void login_dialog::on_connection_result(bool success, const QString& error_message) {
    if (!success) {
        enable_form(true);
        status_label_->setText("");
        status_label_->setStyleSheet("QLabel { color: #c00; }");

        QMessageBox::critical(this, "Connection Failed",
            QString("Failed to connect to server: %1").arg(error_message));

        // Clean up on failure
        work_guard_.reset();  // Allow IO context to finish
        if (io_context_) {
            io_context_->stop();
        }
        if (io_thread_ && io_thread_->joinable()) {
            io_thread_->join();
        }
        io_thread_.reset();
        io_context_.reset();
        client_.reset();
    }
}

void login_dialog::on_login_result(bool success, const QString& error_message) {
    if (success) {
        status_label_->setText("Login successful!");
        status_label_->setStyleSheet("QLabel { color: #0a0; }");
        accept();  // Close dialog with success
    } else {
        enable_form(true);
        status_label_->setText("");

        QMessageBox::critical(this, "Login Failed",
            QString("Authentication failed: %1").arg(error_message));

        // Clean up on failure
        work_guard_.reset();  // Allow IO context to finish
        if (io_context_) {
            io_context_->stop();
        }
        if (io_thread_ && io_thread_->joinable()) {
            io_thread_->join();
        }
        io_thread_.reset();
        io_context_.reset();
        client_.reset();
    }
}

}
