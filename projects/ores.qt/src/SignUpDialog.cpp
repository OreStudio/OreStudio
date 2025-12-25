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
#include "ores.qt/SignUpDialog.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QSizePolicy>
#include <QRegularExpression>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::telemetry::log;

SignUpDialog::SignUpDialog(ClientManager* clientManager, QWidget* parent)
    : QDialog(parent),
      username_edit_(new QLineEdit(this)),
      email_edit_(new QLineEdit(this)),
      password_edit_(new QLineEdit(this)),
      confirm_password_edit_(new QLineEdit(this)),
      host_edit_(new QLineEdit(this)),
      port_spinbox_(new QSpinBox(this)),
      signup_button_(new QPushButton("Sign Up", this)),
      cancel_button_(new QPushButton("Cancel", this)),
      status_label_(new QLabel(this)),
      clientManager_(clientManager) {

    setupUI();

    // Connect signals
    connect(signup_button_, &QPushButton::clicked, this, &SignUpDialog::onSignUpClicked);
    connect(cancel_button_, &QPushButton::clicked, this, &QDialog::reject);
    connect(this, &SignUpDialog::signupCompleted,
            this, &SignUpDialog::onSignUpResult);

    // Register SignupResult for cross-thread signal/slot
    qRegisterMetaType<SignupResult>("SignupResult");
}

SignUpDialog::~SignUpDialog() {
}

void SignUpDialog::setupUI() {
    BOOST_LOG_SEV(lg(), debug) << "Setting up UI.";

    setWindowTitle("Sign Up for ORE Studio");
    setModal(true);
    setMinimumWidth(500);
    setFixedWidth(500);
    setSizeGripEnabled(false);

    // Create form layout
    auto* form_layout = new QFormLayout();
    form_layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    // Username field
    username_edit_->setPlaceholderText("Choose a username");
    form_layout->addRow("Username:", username_edit_);

    // Email field
    email_edit_->setPlaceholderText("Enter your email address");
    form_layout->addRow("Email:", email_edit_);

    // Password field
    password_edit_->setEchoMode(QLineEdit::Password);
    password_edit_->setPlaceholderText("Choose a password (min 12 chars)");
    form_layout->addRow("Password:", password_edit_);

    // Confirm password field
    confirm_password_edit_->setEchoMode(QLineEdit::Password);
    confirm_password_edit_->setPlaceholderText("Confirm your password");
    form_layout->addRow("Confirm Password:", confirm_password_edit_);

    // Server host field
    host_edit_->setText("localhost");
    host_edit_->setPlaceholderText("Server hostname or IP");
    form_layout->addRow("Server Host:", host_edit_);

    // Server port field
    port_spinbox_->setRange(1, 65535);
    port_spinbox_->setValue(55555);
    port_spinbox_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    port_spinbox_->setMinimumWidth(username_edit_->minimumSizeHint().width());
    form_layout->addRow("Server Port:", port_spinbox_);

    // Status label
    status_label_->setWordWrap(true);
    status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Set icons on buttons
    const QColor iconColor(220, 220, 220);
    signup_button_->setIcon(IconUtils::createRecoloredIcon(":/icons/ic_fluent_person_add_20_regular.svg", iconColor));
    cancel_button_->setIcon(IconUtils::createRecoloredIcon(":/icons/ic_fluent_dismiss_20_regular.svg", iconColor));

    // Button layout
    auto* button_layout = new QHBoxLayout();
    button_layout->addStretch();
    button_layout->addWidget(cancel_button_);
    button_layout->addWidget(signup_button_);

    // Main layout
    auto* main_layout = new QVBoxLayout(this);
    main_layout->addLayout(form_layout);
    main_layout->addWidget(status_label_);
    main_layout->addSpacing(10);
    main_layout->addLayout(button_layout);

    // Set default button
    signup_button_->setDefault(true);
}

void SignUpDialog::setServerInfo(const QString& host, int port) {
    host_edit_->setText(host);
    port_spinbox_->setValue(port);
}

void SignUpDialog::enableForm(bool enabled) {
    BOOST_LOG_SEV(lg(), trace) << "Enable form: " << enabled;

    username_edit_->setEnabled(enabled);
    email_edit_->setEnabled(enabled);
    password_edit_->setEnabled(enabled);
    confirm_password_edit_->setEnabled(enabled);
    host_edit_->setEnabled(enabled);
    port_spinbox_->setEnabled(enabled);
    signup_button_->setEnabled(enabled);
}

bool SignUpDialog::validateInput() {
    const auto username = username_edit_->text().trimmed();
    const auto email = email_edit_->text().trimmed();
    const auto password = password_edit_->text();
    const auto confirmPassword = confirm_password_edit_->text();
    const auto host = host_edit_->text().trimmed();

    if (username.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a username.");
        username_edit_->setFocus();
        return false;
    }

    if (email.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter an email address.");
        email_edit_->setFocus();
        return false;
    }

    const QRegularExpression emailRegex(R"(.+@.+\..+)");
    if (!emailRegex.match(email).hasMatch()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please enter a valid email address.");
        email_edit_->setFocus();
        return false;
    }

    if (password.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a password.");
        password_edit_->setFocus();
        return false;
    }

    if (password != confirmPassword) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Passwords do not match. Please try again.");
        confirm_password_edit_->setFocus();
        confirm_password_edit_->selectAll();
        return false;
    }

    if (host.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a server host.");
        host_edit_->setFocus();
        return false;
    }

    return true;
}

void SignUpDialog::onSignUpClicked() {
    BOOST_LOG_SEV(lg(), trace) << "On sign up was clicked.";

    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    if (!validateInput()) {
        return;
    }

    const auto username = username_edit_->text().trimmed();
    const auto email = email_edit_->text().trimmed();
    const auto password = password_edit_->text();
    const auto host = host_edit_->text().trimmed();
    const auto port = static_cast<std::uint16_t>(port_spinbox_->value());

    // Disable form during registration
    enableForm(false);
    status_label_->setText("Creating account...");
    status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Perform signup asynchronously via ClientManager
    auto* watcher = new QFutureWatcher<SignupResult>(this);
    connect(watcher, &QFutureWatcher<SignupResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        emit signupCompleted(result);
    });

    QFuture<SignupResult> future = QtConcurrent::run(
        [this, host, port, username, email, password]() -> SignupResult {
            return clientManager_->signup(
                host.toStdString(), port, username.toStdString(),
                email.toStdString(), password.toStdString());
        }
    );

    watcher->setFuture(future);
}

void SignUpDialog::onSignUpResult(const SignupResult& result) {
    BOOST_LOG_SEV(lg(), debug) << "On signup result called.";
    if (result.success) {
        BOOST_LOG_SEV(lg(), info) << "Signup was successful for user: "
                                  << result.username.toStdString();

        registered_username_ = result.username;
        status_label_->setText("Account created successfully!");
        status_label_->setStyleSheet("QLabel { color: #0a0; }");

        MessageBoxHelper::information(this, "Sign Up Successful",
            QString("Your account '%1' has been created. You can now log in with your credentials.")
                .arg(result.username));

        accept();  // Close dialog with success
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Signup failed: "
                                  << result.error_message.toStdString();

        enableForm(true);
        status_label_->setText("");

        MessageBoxHelper::critical(this, "Sign Up Failed",
            QString("Account creation failed: %1").arg(result.error_message));
    }
}

}
