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

#include <QImage>
#include <QPixmap>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QSizePolicy>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::utility::log;

LoginDialog::LoginDialog(ClientManager* clientManager, QWidget* parent)
    : QDialog(parent),
      username_edit_(new QLineEdit(this)),
      password_edit_(new QLineEdit(this)),
      host_edit_(new QLineEdit(this)),
      port_spinbox_(new QSpinBox(this)),
      login_button_(new QPushButton("Login", this)),
      cancel_button_(new QPushButton("Cancel", this)),
      status_label_(new QLabel(this)),
      clientManager_(clientManager) {

    setupUI();

    // Connect signals
    connect(login_button_, &QPushButton::clicked, this, &LoginDialog::onLoginClicked);
    connect(cancel_button_, &QPushButton::clicked, this, &QDialog::reject);
    connect(this, &LoginDialog::loginCompleted,
            this, &LoginDialog::onLoginResult);
}

LoginDialog::~LoginDialog() {
}

void LoginDialog::setupUI() {
    BOOST_LOG_SEV(lg(), debug) << "Setting up UI.";

    setWindowTitle("Login to ORE Studio");
    setModal(true);
    setMinimumWidth(500);
    setFixedWidth(500); // Make dialog non-resizable horizontally
    setSizeGripEnabled(false); // Disable resize grip

    // Create form layout
    auto* form_layout = new QFormLayout();
    form_layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

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
    port_spinbox_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed); // Match line edit width
    port_spinbox_->setMinimumWidth(username_edit_->minimumSizeHint().width()); // Match line edit width exactly
    form_layout->addRow("Server Port:", port_spinbox_);

    // Status label
    status_label_->setWordWrap(true);
    status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Set icons on buttons
    const QColor iconColor(220, 220, 220); // Light gray for dark theme
    login_button_->setIcon(IconUtils::createRecoloredIcon(":/icons/ic_fluent_checkmark_20_regular.svg", iconColor));
    cancel_button_->setIcon(IconUtils::createRecoloredIcon(":/icons/ic_fluent_dismiss_20_regular.svg", iconColor));

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

void LoginDialog::enableForm(bool enabled) {
    BOOST_LOG_SEV(lg(), trace) << "Enable form: " << enabled;

    username_edit_->setEnabled(enabled);
    password_edit_->setEnabled(enabled);
    host_edit_->setEnabled(enabled);
    port_spinbox_->setEnabled(enabled);
    login_button_->setEnabled(enabled);
}

void LoginDialog::onLoginClicked() {
    BOOST_LOG_SEV(lg(), trace) << "On login was clicked.";

    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    const auto username = username_edit_->text().trimmed();
    const auto password = password_edit_->text();
    const auto host = host_edit_->text().trimmed();
    const auto port = static_cast<std::uint16_t>(port_spinbox_->value());

    // Validate input
    if (username.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a username.");
        username_edit_->setFocus();
        return;
    }

    if (password.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a password.");
        password_edit_->setFocus();
        return;
    }

    if (host.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a server host.");
        host_edit_->setFocus();
        return;
    }

    // Disable form during connection
    enableForm(false);
    status_label_->setText("Connecting to server...");
    status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Perform login asynchronously via ClientManager
    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished,
            [this, watcher]() {
        const auto [success, error_msg] = watcher->result();
        watcher->deleteLater();
        emit loginCompleted(success, error_msg);
    });

    QFuture<std::pair<bool, QString>> future = QtConcurrent::run(
        [this, host, port, username, password]() -> std::pair<bool, QString> {
            return clientManager_->connectAndLogin(
                host.toStdString(), port, username.toStdString(), password.toStdString());
        }
    );

    watcher->setFuture(future);
}

void LoginDialog::onLoginResult(bool success, const QString& error_message) {
    BOOST_LOG_SEV(lg(), debug) << "On login result called.";
    if (success) {
        BOOST_LOG_SEV(lg(), debug) << "Login was successful.";
        status_label_->setText("Login successful!");
        status_label_->setStyleSheet("QLabel { color: #0a0; }");
        accept();  // Close dialog with success
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: "
                                  << error_message.toStdString();

        enableForm(true);
        status_label_->setText("");

        MessageBoxHelper::critical(this, "Login Failed",
            QString("Authentication failed: %1").arg(error_message));
    }
}

}