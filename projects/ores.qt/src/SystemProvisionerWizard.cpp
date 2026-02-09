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
#include "ores.qt/SystemProvisionerWizard.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QMessageBox>
#include <QRegularExpression>
#include <QRegularExpressionValidator>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/login_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// ============================================================================
// SystemProvisionerWizard (Main Wizard)
// ============================================================================

SystemProvisionerWizard::SystemProvisionerWizard(
    ClientManager* clientManager,
    QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager) {

    setWindowTitle(tr("System Bootstrap"));
    setMinimumSize(600, 500);
    resize(700, 550);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();
}

void SystemProvisionerWizard::setupPages() {
    setPage(Page_Welcome, new WelcomePage(this));
    setPage(Page_AdminAccount, new AdminAccountPage(this));
    setPage(Page_Complete, new CompletePage(this));

    setStartId(Page_Welcome);
}

void SystemProvisionerWizard::setAdminCredentials(
    const QString& username, const QString& email, const QString& password) {

    adminUsername_ = username;
    adminEmail_ = email;
    adminPassword_ = password;
}

// ============================================================================
// WelcomePage
// ============================================================================

WelcomePage::WelcomePage(SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Welcome"));
    setupUI();
}

void WelcomePage::setupUI() {
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    // Get hostname from client manager
    QString hostname = "the server";
    if (wizard_->clientManager()) {
        auto host = wizard_->clientManager()->connectedHost();
        if (!host.empty()) {
            hostname = QString::fromStdString(host);
        }
    }

    // Welcome header
    auto* welcomeLabel = new QLabel(
        tr("Welcome to <b>%1</b>").arg(hostname), this);
    welcomeLabel->setStyleSheet("font-size: 18pt; margin-bottom: 10px;");
    welcomeLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(welcomeLabel);

    layout->addSpacing(10);

    // Bootstrap mode explanation
    auto* explanationLabel = new QLabel(this);
    explanationLabel->setWordWrap(true);
    explanationLabel->setText(
        tr("This system is in <b>bootstrap mode</b>. This means it has not yet "
           "been initialised and requires setup before it can be used.\n\n"
           "The setup process will guide you through the following steps:"));
    layout->addWidget(explanationLabel);

    // Steps list
    auto* stepsLabel = new QLabel(this);
    stepsLabel->setWordWrap(true);
    stepsLabel->setTextFormat(Qt::RichText);
    stepsLabel->setText(
        tr("<ol>"
           "<li><b>Create Administrator Account</b> - Set up the initial admin "
           "user who will have full access to manage the system.</li>"
           "<li><b>Auto-Login</b> - You will be automatically logged in as the "
           "new administrator.</li>"
           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    // Info box
    auto* infoBox = new QGroupBox(tr("Note"), this);
    auto* infoLayout = new QVBoxLayout(infoBox);
    auto* infoLabel = new QLabel(
        tr("This wizard only appears during initial system setup. Once "
           "complete, you can onboard tenants from "
           "System > Identity > Onboard Tenant."),
        this);
    infoLabel->setWordWrap(true);
    infoLayout->addWidget(infoLabel);
    layout->addWidget(infoBox);
}

// ============================================================================
// AdminAccountPage
// ============================================================================

AdminAccountPage::AdminAccountPage(SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Create Administrator Account"));
    setSubTitle(tr("Set up the initial administrator account for this system. "
                   "This account will have full administrative privileges."));

    setupUI();
}

void AdminAccountPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    // Form for account details
    auto* formLayout = new QFormLayout();
    formLayout->setSpacing(12);

    // Username
    usernameEdit_ = new QLineEdit(this);
    usernameEdit_->setPlaceholderText(tr("Enter username (letters, numbers, underscores)"));
    usernameEdit_->setMaxLength(50);
    // Validator: alphanumeric and underscores, 3-50 characters
    auto* usernameValidator = new QRegularExpressionValidator(
        QRegularExpression("^[a-zA-Z][a-zA-Z0-9_]{2,49}$"), this);
    usernameEdit_->setValidator(usernameValidator);
    formLayout->addRow(tr("Username:"), usernameEdit_);

    // Email
    emailEdit_ = new QLineEdit(this);
    emailEdit_->setPlaceholderText(tr("Enter email address"));
    emailEdit_->setMaxLength(255);
    formLayout->addRow(tr("Email:"), emailEdit_);

    // Password
    passwordEdit_ = new QLineEdit(this);
    passwordEdit_->setPlaceholderText(tr("Enter password (minimum 8 characters)"));
    passwordEdit_->setEchoMode(QLineEdit::Password);
    passwordEdit_->setMaxLength(128);
    formLayout->addRow(tr("Password:"), passwordEdit_);

    // Confirm password with match indicator
    auto* confirmLayout = new QHBoxLayout();
    confirmPasswordEdit_ = new QLineEdit(this);
    confirmPasswordEdit_->setPlaceholderText(tr("Confirm password"));
    confirmPasswordEdit_->setEchoMode(QLineEdit::Password);
    confirmPasswordEdit_->setMaxLength(128);
    confirmLayout->addWidget(confirmPasswordEdit_);

    // Password match indicator
    passwordMatchLabel_ = new QLabel(this);
    passwordMatchLabel_->setFixedWidth(24);
    passwordMatchLabel_->setAlignment(Qt::AlignCenter);
    confirmLayout->addWidget(passwordMatchLabel_);

    formLayout->addRow(tr("Confirm Password:"), confirmLayout);

    // Show password checkbox
    showPasswordCheckbox_ = new QCheckBox(tr("Show password"), this);
    formLayout->addRow("", showPasswordCheckbox_);

    layout->addLayout(formLayout);
    layout->addSpacing(20);

    // Validation message label
    validationLabel_ = new QLabel(this);
    validationLabel_->setWordWrap(true);
    validationLabel_->setStyleSheet("QLabel { color: #cc0000; }");
    layout->addWidget(validationLabel_);

    layout->addStretch();

    // Connect signals
    connect(showPasswordCheckbox_, &QCheckBox::toggled,
            this, &AdminAccountPage::onShowPasswordToggled);
    connect(passwordEdit_, &QLineEdit::textChanged,
            this, &AdminAccountPage::onPasswordChanged);
    connect(confirmPasswordEdit_, &QLineEdit::textChanged,
            this, &AdminAccountPage::onPasswordChanged);

    // Info box
    auto* infoBox = new QGroupBox(tr("Important"), this);
    auto* infoLayout = new QVBoxLayout(infoBox);
    auto* infoLabel = new QLabel(
        tr("This administrator account will be the first user with full system "
           "access. Keep these credentials secure. You can create additional "
           "accounts and manage roles after the system is provisioned."),
        this);
    infoLabel->setWordWrap(true);
    infoLayout->addWidget(infoLabel);
    layout->addWidget(infoBox);

    // Register fields for mandatory completion
    registerField("adminUsername*", usernameEdit_);
    registerField("adminEmail*", emailEdit_);
    registerField("adminPassword*", passwordEdit_);
}

void AdminAccountPage::initializePage() {
    wizard()->setButtonText(QWizard::NextButton, tr("Create"));
}

bool AdminAccountPage::validatePage() {
    // If already created, allow re-advancing without re-creating
    if (accountCreated_) {
        return true;
    }

    validationLabel_->clear();

    const QString username = usernameEdit_->text().trimmed();
    const QString email = emailEdit_->text().trimmed();
    const QString password = passwordEdit_->text();
    const QString confirmPassword = confirmPasswordEdit_->text();

    // Validate username
    if (username.isEmpty()) {
        validationLabel_->setText(tr("Username is required."));
        usernameEdit_->setFocus();
        return false;
    }
    if (username.length() < 3) {
        validationLabel_->setText(tr("Username must be at least 3 characters."));
        usernameEdit_->setFocus();
        return false;
    }
    QRegularExpression usernameRe("^[a-zA-Z][a-zA-Z0-9_]{2,49}$");
    if (!usernameRe.match(username).hasMatch()) {
        validationLabel_->setText(
            tr("Username must start with a letter and contain only letters, "
               "numbers, and underscores."));
        usernameEdit_->setFocus();
        return false;
    }

    // Validate email
    if (email.isEmpty()) {
        validationLabel_->setText(tr("Email is required."));
        emailEdit_->setFocus();
        return false;
    }
    QRegularExpression emailRe(R"(^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$)");
    if (!emailRe.match(email).hasMatch()) {
        validationLabel_->setText(tr("Please enter a valid email address."));
        emailEdit_->setFocus();
        return false;
    }

    // Validate password
    if (password.isEmpty()) {
        validationLabel_->setText(tr("Password is required."));
        passwordEdit_->setFocus();
        return false;
    }
    if (password.length() < 8) {
        validationLabel_->setText(tr("Password must be at least 8 characters."));
        passwordEdit_->setFocus();
        return false;
    }

    // Validate password confirmation
    if (password != confirmPassword) {
        validationLabel_->setText(tr("Passwords do not match."));
        confirmPasswordEdit_->setFocus();
        return false;
    }

    // Store credentials in wizard
    wizard_->setAdminCredentials(username, email, password);

    // Create the administrator account
    BOOST_LOG_SEV(lg(), info) << "Creating administrator account: "
                               << username.toStdString();

    iam::messaging::create_initial_admin_request adminRequest;
    adminRequest.principal = username.toStdString();
    adminRequest.password = password.toStdString();
    adminRequest.email = email.toStdString();

    auto adminResult = wizard_->clientManager()->process_request(
        std::move(adminRequest));

    if (!adminResult) {
        validationLabel_->setText(
            tr("Failed to communicate with server. Please check your "
               "connection and try again."));
        return false;
    }

    if (!adminResult->success) {
        validationLabel_->setText(
            QString::fromStdString(adminResult->error_message));
        return false;
    }

    wizard_->setAdminAccountId(adminResult->account_id);
    BOOST_LOG_SEV(lg(), info) << "Administrator account created: "
        << boost::uuids::to_string(adminResult->account_id);

    // Login as the new administrator
    BOOST_LOG_SEV(lg(), info) << "Logging in as administrator...";

    auto loginResult = wizard_->clientManager()->login(
        username.toStdString(), password.toStdString());

    if (!loginResult.success) {
        validationLabel_->setText(
            tr("Account created but login failed: %1").arg(
                loginResult.error_message));
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "Logged in as administrator successfully.";
    accountCreated_ = true;

    return true;
}

void AdminAccountPage::onShowPasswordToggled(bool checked) {
    auto mode = checked ? QLineEdit::Normal : QLineEdit::Password;
    passwordEdit_->setEchoMode(mode);
    confirmPasswordEdit_->setEchoMode(mode);
}

void AdminAccountPage::onPasswordChanged() {
    updatePasswordMatchIndicator();
}

void AdminAccountPage::updatePasswordMatchIndicator() {
    const QString password = passwordEdit_->text();
    const QString confirm = confirmPasswordEdit_->text();

    if (confirm.isEmpty()) {
        // No input yet, show nothing
        passwordMatchLabel_->clear();
        passwordMatchLabel_->setStyleSheet("");
    } else if (password == confirm) {
        // Passwords match - green checkmark
        passwordMatchLabel_->setText(QString::fromUtf8("\u2713")); // checkmark
        passwordMatchLabel_->setStyleSheet("QLabel { color: #228B22; font-weight: bold; font-size: 14pt; }");
    } else {
        // Passwords don't match - red X
        passwordMatchLabel_->setText(QString::fromUtf8("\u2717")); // cross
        passwordMatchLabel_->setStyleSheet("QLabel { color: #cc0000; font-weight: bold; font-size: 14pt; }");
    }
}

// ============================================================================
// CompletePage
// ============================================================================

CompletePage::CompletePage(SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void CompletePage::setupUI() {
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    // Success header
    auto* headerLabel = new QLabel(
        tr("System bootstrap complete"), this);
    headerLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    headerLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(headerLabel);

    layout->addSpacing(10);

    // Summary label (populated in initializePage)
    summaryLabel_ = new QLabel(this);
    summaryLabel_->setWordWrap(true);
    summaryLabel_->setTextFormat(Qt::RichText);
    summaryLabel_->setAlignment(Qt::AlignLeft | Qt::AlignTop);
    layout->addWidget(summaryLabel_);

    layout->addStretch();

    // Next steps
    auto* nextStepsBox = new QGroupBox(tr("Next Steps"), this);
    auto* nextStepsLayout = new QVBoxLayout(nextStepsBox);
    auto* nextStepsLabel = new QLabel(
        tr("You can now onboard tenants from "
           "<b>System > Identity > Onboard Tenant</b>.\n\n"
           "Use the <b>Data Librarian</b> to publish reference data bundles "
           "to tenants."),
        this);
    nextStepsLabel->setWordWrap(true);
    nextStepsLabel->setTextFormat(Qt::RichText);
    nextStepsLayout->addWidget(nextStepsLabel);
    layout->addWidget(nextStepsBox);
}

void CompletePage::initializePage() {
    const QString username = wizard_->adminUsername();

    summaryLabel_->setText(
        tr("<p>The administrator account has been created and you are now "
           "logged in.</p>"
           "<p><b>Logged in as:</b> %1</p>")
        .arg(username));

    emit wizard_->provisioningCompleted(username);
}

}
