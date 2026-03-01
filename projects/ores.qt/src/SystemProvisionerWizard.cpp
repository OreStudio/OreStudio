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
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PasswordMatchIndicator.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QMessageBox>
#include <QRegularExpression>
#include <QRegularExpressionValidator>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"

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

    setWindowTitle(tr("New System Provisioner"));
    setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Settings, IconUtils::DefaultIconColor));
    setMinimumSize(900, 700);
    resize(900, 700);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();
}

void SystemProvisionerWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Welcome, new WelcomePage(this));
    setPage(Page_AdminAccount, new AdminAccountPage(this));
    setPage(Page_SetupMode, new SetupModePage(this));
    setPage(Page_TenantDetails, new ProvisionerTenantDetailsPage(this));
    setPage(Page_TenantAdmin, new ProvisionerTenantAdminPage(this));
    setPage(Page_Apply, new ProvisionerApplyPage(this));
    setPage(Page_Complete, new ProvisionerCompletePage(this));

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
    WidgetUtils::setupComboBoxes(this);
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
           "<li><b>Choose Setup Mode</b> - Select single-tenant or "
           "multi-tenant configuration.</li>"
           "<li><b>Create First Tenant</b> - Configure and provision the "
           "first tenant with its own admin account.</li>"
           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    // Info box
    auto* infoBox = new QGroupBox(tr("Note"), this);
    auto* infoLayout = new QVBoxLayout(infoBox);
    auto* infoLabel = new QLabel(
        tr("This wizard only appears during initial system setup. Once "
           "complete, you can provision additional tenants from the "
           "Tenants list window."),
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
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);

    // Form for account details
    auto* formLayout = new QFormLayout();
    formLayout->setSpacing(12);

    // Username
    usernameEdit_ = new QLineEdit(this);
    usernameEdit_->setPlaceholderText(tr("Enter username (letters, numbers, underscores)"));
    usernameEdit_->setMaxLength(50);
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
    PasswordMatchIndicator::connectFields(passwordEdit_, confirmPasswordEdit_);

    // Real-time email validation: red border until format is valid
    static const QRegularExpression emailBorderRe(
        R"(^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9._-]+\.[a-zA-Z]{2,}$)");
    connect(emailEdit_, &QLineEdit::textChanged, this, [this](const QString& text) {
        if (text.isEmpty()) {
            emailEdit_->setStyleSheet({});
            return;
        }
        const bool valid = emailBorderRe.match(text.trimmed()).hasMatch();
        emailEdit_->setStyleSheet(valid ? QString{}
            : QStringLiteral("QLineEdit { border: 1px solid #cc0000; }"));
    });

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
    wizard()->setButtonText(QWizard::NextButton, tr("Create && Continue"));

    // Pre-fill from login credentials if available (bootstrap convenience)
    const auto username = wizard_->adminUsername();
    const auto password = wizard_->adminPassword();

    if (!username.isEmpty() && usernameEdit_->text().isEmpty()) {
        usernameEdit_->setText(username);
        if (emailEdit_->text().isEmpty()) {
            emailEdit_->setText(username + "@localhost.com");
        }
    }
    if (!password.isEmpty() && passwordEdit_->text().isEmpty()) {
        passwordEdit_->setText(password);
        confirmPasswordEdit_->setText(password);
    }
}

int AdminAccountPage::nextId() const {
    return SystemProvisionerWizard::Page_SetupMode;
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
    static const QRegularExpression emailRe(
        R"(^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9._-]+\.[a-zA-Z]{2,}$)");
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

// ============================================================================
// SetupModePage
// ============================================================================

SetupModePage::SetupModePage(SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Setup Mode"));
    setSubTitle(tr("Choose how you want to configure your ORE Studio instance."));

    setupUI();
}

void SetupModePage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    // Single-tenant option
    singleTenantRadio_ = new QRadioButton(tr("Single-Tenant"), this);
    singleTenantRadio_->setChecked(true);
    singleTenantRadio_->setStyleSheet("font-weight: bold; font-size: 11pt;");
    layout->addWidget(singleTenantRadio_);

    auto* singleDesc = new QLabel(
        tr("Best for evaluation, development, or single-organisation deployments. "
           "Creates a default tenant with pre-configured settings. You can always "
           "add more tenants later."),
        this);
    singleDesc->setWordWrap(true);
    singleDesc->setContentsMargins(25, 0, 0, 15);
    layout->addWidget(singleDesc);

    // Multi-tenant option
    multiTenantRadio_ = new QRadioButton(tr("Multi-Tenant"), this);
    multiTenantRadio_->setStyleSheet("font-weight: bold; font-size: 11pt;");
    layout->addWidget(multiTenantRadio_);

    auto* multiDesc = new QLabel(
        tr("For production deployments serving multiple organisations. "
           "You will configure the first tenant's details on the next page. "
           "Additional tenants can be onboarded from the Tenants window."),
        this);
    multiDesc->setWordWrap(true);
    multiDesc->setContentsMargins(25, 0, 0, 15);
    layout->addWidget(multiDesc);

    layout->addStretch();
}

void SetupModePage::initializePage() {
    wizard()->setButtonText(QWizard::NextButton, tr("Next >"));
}

bool SetupModePage::validatePage() {
    const bool multiTenant = multiTenantRadio_->isChecked();
    wizard_->setMultiTenantMode(multiTenant);

    if (!multiTenant) {
        // Single-tenant: auto-fill defaults
        wizard_->setTenantCode("default");
        wizard_->setTenantName("Default Tenant");
        wizard_->setTenantType("evaluation");
        wizard_->setTenantHostname("localhost");
        wizard_->setTenantDescription("Default tenant for single-tenant deployment");
    }

    return true;
}

// ============================================================================
// ProvisionerTenantDetailsPage
// ============================================================================

ProvisionerTenantDetailsPage::ProvisionerTenantDetailsPage(
    SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Tenant Details"));
    setSubTitle(tr("Configure the first tenant for your ORE Studio instance."));

    setupUI();
}

void ProvisionerTenantDetailsPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);

    auto* formLayout = new QFormLayout();
    formLayout->setSpacing(12);

    // Code
    codeEdit_ = new QLineEdit(this);
    codeEdit_->setPlaceholderText(tr("Unique tenant code (e.g., acme)"));
    codeEdit_->setMaxLength(50);
    auto* codeValidator = new QRegularExpressionValidator(
        QRegularExpression("^[a-z][a-z0-9_]{0,49}$"), this);
    codeEdit_->setValidator(codeValidator);
    formLayout->addRow(tr("Code:"), codeEdit_);

    // Name
    nameEdit_ = new QLineEdit(this);
    nameEdit_->setPlaceholderText(tr("Display name for the tenant"));
    nameEdit_->setMaxLength(200);
    formLayout->addRow(tr("Name:"), nameEdit_);

    // Type
    typeCombo_ = new QComboBox(this);
    typeCombo_->addItems({"evaluation", "production", "automation"});
    formLayout->addRow(tr("Type:"), typeCombo_);

    // Hostname
    hostnameEdit_ = new QLineEdit(this);
    hostnameEdit_->setPlaceholderText(tr("Auto-generated from code"));
    hostnameEdit_->setMaxLength(255);
    formLayout->addRow(tr("Hostname:"), hostnameEdit_);

    // Description
    descriptionEdit_ = new QLineEdit(this);
    descriptionEdit_->setPlaceholderText(tr("Optional description"));
    descriptionEdit_->setMaxLength(500);
    formLayout->addRow(tr("Description:"), descriptionEdit_);

    layout->addLayout(formLayout);
    layout->addSpacing(20);

    // Validation label
    validationLabel_ = new QLabel(this);
    validationLabel_->setWordWrap(true);
    validationLabel_->setStyleSheet("QLabel { color: #cc0000; }");
    layout->addWidget(validationLabel_);

    layout->addStretch();

    // Auto-generate hostname from code
    connect(codeEdit_, &QLineEdit::textChanged,
            this, &ProvisionerTenantDetailsPage::onCodeChanged);

    // Track manual hostname edits
    connect(hostnameEdit_, &QLineEdit::textEdited,
            this, [this]() { hostnameManuallyEdited_ = true; });

    // Register required fields
    registerField("tenantCode*", codeEdit_);
    registerField("tenantName*", nameEdit_);
}

void ProvisionerTenantDetailsPage::initializePage() {
    wizard()->setButtonText(QWizard::NextButton, tr("Next >"));

    // Pre-fill from wizard state (single-tenant mode sets defaults)
    if (codeEdit_->text().isEmpty() && !wizard_->tenantCode().isEmpty()) {
        codeEdit_->setText(wizard_->tenantCode());
    }
    if (nameEdit_->text().isEmpty() && !wizard_->tenantName().isEmpty()) {
        nameEdit_->setText(wizard_->tenantName());
    }
    if (!wizard_->tenantType().isEmpty()) {
        int idx = typeCombo_->findText(wizard_->tenantType());
        if (idx >= 0) typeCombo_->setCurrentIndex(idx);
    }
    if (hostnameEdit_->text().isEmpty() && !wizard_->tenantHostname().isEmpty()) {
        hostnameEdit_->setText(wizard_->tenantHostname());
    }
    if (descriptionEdit_->text().isEmpty() && !wizard_->tenantDescription().isEmpty()) {
        descriptionEdit_->setText(wizard_->tenantDescription());
    }

    // In single-tenant mode, make fields read-only
    if (!wizard_->isMultiTenantMode()) {
        codeEdit_->setReadOnly(true);
        nameEdit_->setReadOnly(true);
        typeCombo_->setEnabled(false);
        hostnameEdit_->setReadOnly(true);
    } else {
        codeEdit_->setReadOnly(false);
        nameEdit_->setReadOnly(false);
        typeCombo_->setEnabled(true);
        hostnameEdit_->setReadOnly(false);
    }
}

void ProvisionerTenantDetailsPage::onCodeChanged(const QString& text) {
    if (!hostnameManuallyEdited_ && !text.isEmpty()) {
        hostnameEdit_->setText(text);
    }
}

bool ProvisionerTenantDetailsPage::validatePage() {
    validationLabel_->clear();

    const QString code = codeEdit_->text().trimmed();
    const QString name = nameEdit_->text().trimmed();
    const QString hostname = hostnameEdit_->text().trimmed();

    if (code.isEmpty()) {
        validationLabel_->setText(tr("Tenant code is required."));
        codeEdit_->setFocus();
        return false;
    }

    QRegularExpression codeRe("^[a-z][a-z0-9_]{0,49}$");
    if (!codeRe.match(code).hasMatch()) {
        validationLabel_->setText(
            tr("Code must start with a lowercase letter and contain only "
               "lowercase letters, numbers, and underscores."));
        codeEdit_->setFocus();
        return false;
    }

    if (name.isEmpty()) {
        validationLabel_->setText(tr("Tenant name is required."));
        nameEdit_->setFocus();
        return false;
    }

    if (hostname.isEmpty()) {
        validationLabel_->setText(tr("Hostname is required."));
        hostnameEdit_->setFocus();
        return false;
    }

    // Store in wizard
    wizard_->setTenantCode(code);
    wizard_->setTenantName(name);
    wizard_->setTenantType(typeCombo_->currentText());
    wizard_->setTenantHostname(hostname);
    wizard_->setTenantDescription(descriptionEdit_->text().trimmed());

    return true;
}

// ============================================================================
// ProvisionerTenantAdminPage
// ============================================================================

ProvisionerTenantAdminPage::ProvisionerTenantAdminPage(
    SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Tenant Administrator Account"));
    setSubTitle(tr("Create the initial administrator account for the new tenant."));

    setupUI();
}

void ProvisionerTenantAdminPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);

    auto* formLayout = new QFormLayout();
    formLayout->setSpacing(12);

    // Username
    usernameEdit_ = new QLineEdit(this);
    usernameEdit_->setPlaceholderText(tr("Tenant admin username"));
    usernameEdit_->setMaxLength(50);
    formLayout->addRow(tr("Username:"), usernameEdit_);

    // Email
    emailEdit_ = new QLineEdit(this);
    emailEdit_->setPlaceholderText(tr("Tenant admin email"));
    emailEdit_->setMaxLength(255);
    formLayout->addRow(tr("Email:"), emailEdit_);

    // Password
    passwordEdit_ = new QLineEdit(this);
    passwordEdit_->setPlaceholderText(tr("Password (minimum 8 characters)"));
    passwordEdit_->setEchoMode(QLineEdit::Password);
    passwordEdit_->setMaxLength(128);
    formLayout->addRow(tr("Password:"), passwordEdit_);

    // Confirm password
    auto* confirmLayout = new QHBoxLayout();
    confirmPasswordEdit_ = new QLineEdit(this);
    confirmPasswordEdit_->setPlaceholderText(tr("Confirm password"));
    confirmPasswordEdit_->setEchoMode(QLineEdit::Password);
    confirmPasswordEdit_->setMaxLength(128);
    confirmLayout->addWidget(confirmPasswordEdit_);
    formLayout->addRow(tr("Confirm Password:"), confirmLayout);

    // Show password
    showPasswordCheck_ = new QCheckBox(tr("Show password"), this);
    formLayout->addRow("", showPasswordCheck_);

    layout->addLayout(formLayout);
    layout->addSpacing(20);

    // Validation label
    validationLabel_ = new QLabel(this);
    validationLabel_->setWordWrap(true);
    validationLabel_->setStyleSheet("QLabel { color: #cc0000; }");
    layout->addWidget(validationLabel_);

    layout->addStretch();

    connect(showPasswordCheck_, &QCheckBox::toggled,
            this, &ProvisionerTenantAdminPage::onShowPasswordToggled);
    PasswordMatchIndicator::connectFields(passwordEdit_, confirmPasswordEdit_);

    // Real-time email validation: red border until format is valid
    static const QRegularExpression emailBorderRe(
        R"(^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9._-]+\.[a-zA-Z]{2,}$)");
    connect(emailEdit_, &QLineEdit::textChanged, this, [this](const QString& text) {
        if (text.isEmpty()) {
            emailEdit_->setStyleSheet({});
            return;
        }
        const bool valid = emailBorderRe.match(text.trimmed()).hasMatch();
        emailEdit_->setStyleSheet(valid ? QString{}
            : QStringLiteral("QLineEdit { border: 1px solid #cc0000; }"));
    });

    registerField("tenantAdminUsername*", usernameEdit_);
    registerField("tenantAdminEmail*", emailEdit_);
    registerField("tenantAdminPassword*", passwordEdit_);
}

void ProvisionerTenantAdminPage::initializePage() {
    wizard()->setButtonText(QWizard::NextButton, tr("Provision Tenant"));

    // Pre-fill defaults
    if (usernameEdit_->text().isEmpty()) {
        usernameEdit_->setText("tenant_admin");
    }
    if (emailEdit_->text().isEmpty()) {
        const auto code = wizard_->tenantCode();
        emailEdit_->setText(QString("admin@%1.com").arg(
            code.isEmpty() ? "tenant" : code));
    }
}

bool ProvisionerTenantAdminPage::validatePage() {
    validationLabel_->clear();

    const QString username = usernameEdit_->text().trimmed();
    const QString email = emailEdit_->text().trimmed();
    const QString password = passwordEdit_->text();
    const QString confirmPassword = confirmPasswordEdit_->text();

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

    if (email.isEmpty()) {
        validationLabel_->setText(tr("Email is required."));
        emailEdit_->setFocus();
        return false;
    }
    static const QRegularExpression emailRe(
        R"(^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9._-]+\.[a-zA-Z]{2,}$)");
    if (!emailRe.match(email).hasMatch()) {
        validationLabel_->setText(tr("Please enter a valid email address."));
        emailEdit_->setFocus();
        return false;
    }

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
    if (password != confirmPassword) {
        validationLabel_->setText(tr("Passwords do not match."));
        confirmPasswordEdit_->setFocus();
        return false;
    }

    wizard_->setTenantAdminUsername(username);
    wizard_->setTenantAdminPassword(password);
    wizard_->setTenantAdminEmail(email);

    return true;
}

void ProvisionerTenantAdminPage::onShowPasswordToggled(bool checked) {
    auto mode = checked ? QLineEdit::Normal : QLineEdit::Password;
    passwordEdit_->setEchoMode(mode);
    confirmPasswordEdit_->setEchoMode(mode);
}

// ============================================================================
// ProvisionerApplyPage
// ============================================================================

ProvisionerApplyPage::ProvisionerApplyPage(SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Provisioning Tenant"));
    setFinalPage(false);

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(tr("Starting..."), this);
    statusLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0); // indeterminate
    progressBar_->setTextVisible(false);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");
    layout->addWidget(progressBar_);

    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setFont(FontUtils::monospace());
    layout->addWidget(logOutput_);
}

bool ProvisionerApplyPage::isComplete() const {
    return provisioningComplete_;
}

void ProvisionerApplyPage::initializePage() {
    wizard()->setButtonText(QWizard::NextButton, tr("Continue"));

    provisioningComplete_ = false;
    provisioningSuccess_ = false;
    logOutput_->clear();
    statusLabel_->setText(tr("Provisioning tenant..."));
    progressBar_->setRange(0, 0);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");

    startProvisioning();
}

void ProvisionerApplyPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void ProvisionerApplyPage::startProvisioning() {
    const std::string code = wizard_->tenantCode().toStdString();
    const std::string name = wizard_->tenantName().toStdString();
    const std::string type = wizard_->tenantType().toStdString();
    const std::string hostname = wizard_->tenantHostname().toStdString();
    const std::string description = wizard_->tenantDescription().toStdString();
    const std::string adminUsername = wizard_->tenantAdminUsername().toStdString();
    const std::string adminPassword = wizard_->tenantAdminPassword().toStdString();
    const std::string adminEmail = wizard_->tenantAdminEmail().toStdString();
    ClientManager* clientManager = wizard_->clientManager();

    struct ProvisioningResult {
        bool success = false;
        std::string error;
        std::string tenantId;
    };

    auto* watcher = new QFutureWatcher<ProvisioningResult>(this);
    connect(watcher, &QFutureWatcher<ProvisioningResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!result.success) {
            statusLabel_->setText(tr("Provisioning failed"));
            appendLog(tr("ERROR: %1").arg(
                QString::fromStdString(result.error)));
            progressBar_->setRange(0, 1);
            progressBar_->setValue(1);
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");
        } else {
            statusLabel_->setText(tr("Provisioning complete"));
            wizard_->setProvisionedTenantId(
                QString::fromStdString(result.tenantId));
            appendLog(tr("Tenant '%1' provisioned successfully (ID: %2).")
                .arg(wizard_->tenantName(),
                     QString::fromStdString(result.tenantId)));
            appendLog(tr("Admin account '%1' created with TenantAdmin role.")
                .arg(wizard_->tenantAdminUsername()));
            progressBar_->setRange(0, 1);
            progressBar_->setValue(1);
            provisioningSuccess_ = true;
        }

        provisioningComplete_ = true;
        emit completeChanged();
    });

    QFuture<ProvisioningResult> future = QtConcurrent::run(
        [clientManager, code, name, type, hostname, description,
         adminUsername, adminPassword, adminEmail]() -> ProvisioningResult {

            ProvisioningResult result;

            iam::messaging::provision_tenant_request request;
            request.type = type;
            request.code = code;
            request.name = name;
            request.hostname = hostname;
            request.description = description;
            request.admin_username = adminUsername;
            request.admin_password = adminPassword;
            request.admin_email = adminEmail;

            auto response = clientManager->process_authenticated_request(
                std::move(request));

            if (!response) {
                result.error = "Failed to communicate with server";
                return result;
            }

            if (!response->success) {
                result.error = response->error_message;
                return result;
            }

            result.success = true;
            result.tenantId = response->tenant_id;
            return result;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("Provisioning tenant '%1' (code: %2, hostname: %3)...")
        .arg(wizard_->tenantName(), wizard_->tenantCode(),
             wizard_->tenantHostname()));
    appendLog(tr("Creating admin account '%1' for new tenant...")
        .arg(wizard_->tenantAdminUsername()));
}

// ============================================================================
// ProvisionerCompletePage
// ============================================================================

ProvisionerCompletePage::ProvisionerCompletePage(SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void ProvisionerCompletePage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    // Success header
    auto* headerLabel = new QLabel(
        tr("System provisioning complete"), this);
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
    nextStepsLabel_ = new QLabel(
        tr("Log in as <b>%1@%2</b> to start the tenant provisioning wizard, "
           "which will help you set up reference data and party hierarchies.\n\n"
           "Use the <b>Data Librarian</b> to publish reference data bundles "
           "to tenants.").arg("tenant_admin", "hostname"),
        this);
    nextStepsLabel_->setWordWrap(true);
    nextStepsLabel_->setTextFormat(Qt::RichText);
    nextStepsLayout->addWidget(nextStepsLabel_);
    layout->addWidget(nextStepsBox);
}

void ProvisionerCompletePage::initializePage() {
    const QString adminUsername = wizard_->adminUsername();
    const QString tenantName = wizard_->tenantName();
    const QString tenantCode = wizard_->tenantCode();
    const QString tenantHostname = wizard_->tenantHostname();
    const QString tenantAdminUsername = wizard_->tenantAdminUsername();

    summaryLabel_->setText(
        tr("<p>The system has been bootstrapped and the first tenant has been "
           "provisioned.</p>"
           "<p><b>System admin:</b> %1</p>"
           "<p><b>Tenant:</b> %2 (code: %3)</p>"
           "<p><b>Tenant admin:</b> %4@%5</p>")
        .arg(adminUsername, tenantName, tenantCode,
             tenantAdminUsername, tenantHostname));

    // Update next steps with actual values
    if (nextStepsLabel_) {
        if (wizard_->isMultiTenantMode()) {
            nextStepsLabel_->setText(
                tr("Log in as <b>%1@%2</b> to start the tenant provisioning "
                   "wizard, which will help you set up reference data.\n\n"
                   "To create additional tenants, use <b>System > Identity > "
                   "Tenants</b> and click <b>Onboard</b>.")
                .arg(tenantAdminUsername, tenantHostname));
        } else {
            nextStepsLabel_->setText(
                tr("Log in as <b>%1@%2</b> to start the tenant provisioning "
                   "wizard, which will help you set up reference data and "
                   "party hierarchies.")
                .arg(tenantAdminUsername, tenantHostname));
        }
    }

    emit wizard_->provisioningCompleted(adminUsername);
}

}
