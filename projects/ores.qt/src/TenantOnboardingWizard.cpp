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
#include "ores.qt/TenantOnboardingWizard.hpp"
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/PasswordMatchIndicator.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QButtonGroup>
#include <QMessageBox>
#include <QRegularExpression>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/LeiEntityPicker.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// ============================================================================
// TenantOnboardingWizard (Main Wizard)
// ============================================================================

TenantOnboardingWizard::TenantOnboardingWizard(
    ClientManager* clientManager,
    QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager) {

    setWindowTitle(tr("Tenant Provisioning"));
    setMinimumSize(800, 650);
    resize(900, 700);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();
}

void TenantOnboardingWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_ModeAndLei, new ModeAndLeiPage(this));
    setPage(Page_TenantDetails, new TenantDetailsPage(this));
    setPage(Page_AdminAccount, new OnboardingAdminAccountPage(this));
    setPage(Page_Apply, new ApplyOnboardingPage(this));

    setStartId(Page_ModeAndLei);
}

// ============================================================================
// ModeAndLeiPage
// ============================================================================

ModeAndLeiPage::ModeAndLeiPage(TenantOnboardingWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Provisioning Mode"));
    setSubTitle(tr("Choose whether to create a blank tenant or "
                   "pre-fill details from a GLEIF LEI entity."));
    setupUI();
}

void ModeAndLeiPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(6);

    // Mode selection
    auto* modeGroup = new QGroupBox(tr("Tenant Mode"), this);
    auto* modeLayout = new QVBoxLayout(modeGroup);
    modeLayout->setContentsMargins(8, 8, 8, 8);

    blankRadio_ = new QRadioButton(
        tr("Blank Tenant - Create an empty tenant"), this);
    gleifRadio_ = new QRadioButton(
        tr("GLEIF-Based Tenant - Pre-fill details from LEI entity"), this);
    blankRadio_->setChecked(true);

    auto* buttonGroup = new QButtonGroup(this);
    buttonGroup->addButton(blankRadio_);
    buttonGroup->addButton(gleifRadio_);

    modeLayout->addWidget(blankRadio_);
    modeLayout->addWidget(gleifRadio_);
    layout->addWidget(modeGroup, 0);

    connect(blankRadio_, &QRadioButton::toggled, this,
            &ModeAndLeiPage::onModeChanged);

    // Informational blurb about evaluation tenants
    auto* infoLabel = new QLabel(
        tr("<b>Note:</b> This wizard provisions a new tenant "
           "\u2014 an isolated environment with its own users, roles, "
           "and reference data. Choose <i>Evaluation</i> for demos, QA, "
           "and testing, or <i>Production</i> for live workloads."
           "<br><br>"
           "If you prefer a simpler single-tenant setup, you can use the "
           "system tenant directly without creating separate tenants. This "
           "avoids the overhead of multi-tenancy but limits isolation between "
           "environments."),
        this);
    infoLabel->setWordWrap(true);
    infoLabel->setTextFormat(Qt::RichText);
    infoLabel->setStyleSheet(
        "QLabel {"
        "  background-color: #2a2d32;"
        "  border: 1px solid #444950;"
        "  border-radius: 4px;"
        "  padding: 10px;"
        "  color: #b0b8c4;"
        "}");
    layout->addWidget(infoLabel, 0);

    // LEI entity picker (disabled until GLEIF mode)
    leiPicker_ = new LeiEntityPicker(wizard_->clientManager(), this);
    leiPicker_->setEnabled(false);
    layout->addWidget(leiPicker_, 1);

    // Selected entity feedback
    selectedEntityLabel_ = new QLabel(this);
    selectedEntityLabel_->setWordWrap(true);
    selectedEntityLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(selectedEntityLabel_, 0);

    connect(leiPicker_, &LeiEntityPicker::entitySelected,
            this, [this](const QString& lei, const QString& name) {
        selectedEntityLabel_->setText(
            tr("Selected: %1 (%2)").arg(name, lei));
        emit completeChanged();
    });

    connect(leiPicker_, &LeiEntityPicker::selectionCleared,
            this, [this]() {
        selectedEntityLabel_->clear();
        emit completeChanged();
    });
}

void ModeAndLeiPage::onModeChanged() {
    const bool gleif = gleifRadio_->isChecked();
    leiPicker_->setEnabled(gleif);

    if (gleif && !leiLoaded_) {
        leiPicker_->load();
        leiLoaded_ = true;
    }
}

void ModeAndLeiPage::initializePage() {
    // Load LEI data on first visit if already in GLEIF mode
    if (gleifRadio_->isChecked() && !leiLoaded_) {
        leiPicker_->load();
        leiLoaded_ = true;
    }
}

bool ModeAndLeiPage::validatePage() {
    const bool gleif = gleifRadio_->isChecked();
    wizard_->setGleifMode(gleif);

    if (gleif) {
        if (!leiPicker_->hasSelection()) {
            QMessageBox::warning(this, tr("No Selection"),
                tr("Please select an LEI entity to use as the root party."));
            return false;
        }

        wizard_->setRootLei(leiPicker_->selectedLei());
        wizard_->setRootLeiName(leiPicker_->selectedName());
    }

    return true;
}

int ModeAndLeiPage::nextId() const {
    return TenantOnboardingWizard::Page_TenantDetails;
}

// ============================================================================
// TenantDetailsPage
// ============================================================================

TenantDetailsPage::TenantDetailsPage(TenantOnboardingWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Tenant Details"));
    setSubTitle(tr("Configure the tenant identity. In GLEIF mode, fields are "
                   "pre-filled from the selected LEI entity but can be edited."));
    setupUI();
}

void TenantDetailsPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);

    auto* formLayout = new QFormLayout();
    formLayout->setSpacing(10);

    codeEdit_ = new QLineEdit(this);
    codeEdit_->setPlaceholderText(tr("Unique tenant code (e.g., acme)"));
    codeEdit_->setMaxLength(100);
    formLayout->addRow(tr("Code:"), codeEdit_);

    nameEdit_ = new QLineEdit(this);
    nameEdit_->setPlaceholderText(tr("Display name (e.g., ACME Corporation)"));
    nameEdit_->setMaxLength(255);
    formLayout->addRow(tr("Name:"), nameEdit_);

    typeCombo_ = new QComboBox(this);
    typeCombo_->addItem(tr("Evaluation"), "evaluation");
    typeCombo_->addItem(tr("Production"), "production");
    formLayout->addRow(tr("Type:"), typeCombo_);

    hostnameEdit_ = new QLineEdit(this);
    hostnameEdit_->setPlaceholderText(
        tr("Unique hostname (e.g., acme)"));
    hostnameEdit_->setMaxLength(255);
    formLayout->addRow(tr("Hostname:"), hostnameEdit_);

    descriptionEdit_ = new QLineEdit(this);
    descriptionEdit_->setPlaceholderText(tr("Optional description"));
    descriptionEdit_->setMaxLength(500);
    formLayout->addRow(tr("Description:"), descriptionEdit_);

    layout->addLayout(formLayout);

    // Validation label
    validationLabel_ = new QLabel(this);
    validationLabel_->setWordWrap(true);
    validationLabel_->setStyleSheet("QLabel { color: #cc0000; }");
    layout->addWidget(validationLabel_);

    layout->addStretch();

    // Auto-generate hostname from code
    connect(codeEdit_, &QLineEdit::textChanged,
            this, &TenantDetailsPage::onCodeChanged);

    // Track manual hostname edits
    connect(hostnameEdit_, &QLineEdit::textEdited, this, [this]() {
        hostnameManuallyEdited_ = true;
    });

    // Register mandatory fields
    registerField("tenantCode*", codeEdit_);
    registerField("tenantName*", nameEdit_);
}

void TenantDetailsPage::initializePage() {
    // Reset manual hostname tracking when entering this page
    hostnameManuallyEdited_ = false;

    if (wizard_->isGleifMode() && !wizard_->rootLeiName().isEmpty()) {
        const QString entityName = wizard_->rootLeiName();

        // Pre-fill name from LEI entity legal name
        nameEdit_->setText(entityName);

        // Generate a code from the entity name: lowercase, replace spaces
        // with underscores, remove non-alphanumeric characters
        QString code = entityName.toLower();
        code.replace(QRegularExpression("[^a-z0-9]+"), "_");
        code.replace(QRegularExpression("_+"), "_");
        code.remove(QRegularExpression("^_|_$"));
        if (code.length() > 50) {
            code.truncate(50);
            code.remove(QRegularExpression("_$"));
        }
        codeEdit_->setText(code);

        // GLEIF mode: force evaluation type
        typeCombo_->setCurrentIndex(
            typeCombo_->findData(QStringLiteral("evaluation")));
        typeCombo_->setEnabled(false);
    } else {
        // Blank mode: clear fields for fresh entry
        codeEdit_->clear();
        nameEdit_->clear();
        hostnameEdit_->clear();
        descriptionEdit_->clear();
        typeCombo_->setEnabled(true);
    }
}

void TenantDetailsPage::onCodeChanged(const QString& text) {
    updateHostname();
    Q_UNUSED(text);
}

void TenantDetailsPage::updateHostname() {
    if (hostnameManuallyEdited_)
        return;

    const QString code = codeEdit_->text().trimmed().toLower();
    if (!code.isEmpty()) {
        hostnameEdit_->setText(code);
    } else {
        hostnameEdit_->clear();
    }
}

bool TenantDetailsPage::validatePage() {
    validationLabel_->clear();

    const QString code = codeEdit_->text().trimmed();
    const QString name = nameEdit_->text().trimmed();
    const QString hostname = hostnameEdit_->text().trimmed();

    if (code.isEmpty()) {
        validationLabel_->setText(tr("Tenant code is required."));
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

    // Store values in wizard
    wizard_->setTenantCode(code);
    wizard_->setTenantName(name);
    wizard_->setTenantType(
        typeCombo_->currentData().toString());
    wizard_->setTenantHostname(hostname);
    wizard_->setTenantDescription(descriptionEdit_->text().trimmed());

    return true;
}

int TenantDetailsPage::nextId() const {
    return TenantOnboardingWizard::Page_AdminAccount;
}

// ============================================================================
// OnboardingAdminAccountPage
// ============================================================================

OnboardingAdminAccountPage::OnboardingAdminAccountPage(
    TenantOnboardingWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Admin Account"));
    setSubTitle(tr("Create the initial administrator account for the new "
                   "tenant. This account will have the TenantAdmin role."));
    setupUI();
}

void OnboardingAdminAccountPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);

    auto* formLayout = new QFormLayout();
    formLayout->setSpacing(10);

    usernameEdit_ = new QLineEdit(this);
    usernameEdit_->setPlaceholderText(tr("admin"));
    usernameEdit_->setMaxLength(100);
    formLayout->addRow(tr("Username:"), usernameEdit_);

    emailEdit_ = new QLineEdit(this);
    emailEdit_->setPlaceholderText(tr("admin@example.com"));
    emailEdit_->setMaxLength(255);
    formLayout->addRow(tr("Email:"), emailEdit_);

    passwordEdit_ = new QLineEdit(this);
    passwordEdit_->setEchoMode(QLineEdit::Password);
    passwordEdit_->setPlaceholderText(tr("Minimum 8 characters"));
    formLayout->addRow(tr("Password:"), passwordEdit_);

    confirmPasswordEdit_ = new QLineEdit(this);
    confirmPasswordEdit_->setEchoMode(QLineEdit::Password);
    confirmPasswordEdit_->setPlaceholderText(tr("Re-enter password"));
    formLayout->addRow(tr("Confirm:"), confirmPasswordEdit_);

    showPasswordCheck_ = new QCheckBox(tr("Show password"), this);
    formLayout->addRow(QString(), showPasswordCheck_);

    connect(showPasswordCheck_, &QCheckBox::toggled,
            this, &OnboardingAdminAccountPage::onShowPasswordToggled);

    PasswordMatchIndicator::connectFields(passwordEdit_, confirmPasswordEdit_);

    layout->addLayout(formLayout);

    // Validation label
    validationLabel_ = new QLabel(this);
    validationLabel_->setWordWrap(true);
    validationLabel_->setStyleSheet("QLabel { color: #cc0000; }");
    layout->addWidget(validationLabel_);

    layout->addStretch();

    // Register mandatory fields
    registerField("adminUsername*", usernameEdit_);
    registerField("adminEmail*", emailEdit_);
    registerField("adminPassword*", passwordEdit_);
}

void OnboardingAdminAccountPage::initializePage() {
    usernameEdit_->setText(QStringLiteral("tenant_admin"));

    const QString code = wizard_->tenantCode();
    emailEdit_->setText(QStringLiteral("admin@") + code);
}

void OnboardingAdminAccountPage::onShowPasswordToggled(bool checked) {
    const auto mode = checked ? QLineEdit::Normal : QLineEdit::Password;
    passwordEdit_->setEchoMode(mode);
    confirmPasswordEdit_->setEchoMode(mode);
}

bool OnboardingAdminAccountPage::validatePage() {
    validationLabel_->clear();

    const QString username = usernameEdit_->text().trimmed();
    const QString email = emailEdit_->text().trimmed();
    const QString password = passwordEdit_->text();
    const QString confirm = confirmPasswordEdit_->text();

    if (username.isEmpty()) {
        validationLabel_->setText(tr("Username is required."));
        usernameEdit_->setFocus();
        return false;
    }

    if (email.isEmpty()) {
        validationLabel_->setText(tr("Email is required."));
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

    if (password != confirm) {
        validationLabel_->setText(tr("Passwords do not match."));
        confirmPasswordEdit_->setFocus();
        return false;
    }

    // Store values in wizard
    wizard_->setAdminUsername(username);
    wizard_->setAdminPassword(password);
    wizard_->setAdminEmail(email);

    return true;
}

int OnboardingAdminAccountPage::nextId() const {
    return TenantOnboardingWizard::Page_Apply;
}

// ============================================================================
// ApplyOnboardingPage
// ============================================================================

ApplyOnboardingPage::ApplyOnboardingPage(
    TenantOnboardingWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Provisioning Tenant"));
    setFinalPage(true);

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(tr("Starting..."), this);
    statusLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0); // indeterminate
    layout->addWidget(progressBar_);

    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setFont(FontUtils::monospace());
    layout->addWidget(logOutput_);
}

bool ApplyOnboardingPage::isComplete() const {
    return onboardingComplete_;
}

void ApplyOnboardingPage::initializePage() {
    onboardingComplete_ = false;
    onboardingSuccess_ = false;
    logOutput_->clear();
    statusLabel_->setText(tr("Provisioning tenant..."));
    progressBar_->setRange(0, 0);

    startOnboarding();
}

void ApplyOnboardingPage::appendLog(const QString& message) {
    logOutput_->append(message);
    // Scroll to bottom
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void ApplyOnboardingPage::startOnboarding() {
    const std::string code = wizard_->tenantCode().toStdString();
    const std::string name = wizard_->tenantName().toStdString();
    const std::string type = wizard_->tenantType().toStdString();
    const std::string hostname = wizard_->tenantHostname().toStdString();
    const std::string description = wizard_->tenantDescription().toStdString();
    const std::string adminUsername = wizard_->adminUsername().toStdString();
    const std::string adminPassword = wizard_->adminPassword().toStdString();
    const std::string adminEmail = wizard_->adminEmail().toStdString();
    ClientManager* clientManager = wizard_->clientManager();

    struct OnboardingResult {
        bool success = false;
        std::string error;
        std::string tenantId;
    };

    auto* watcher = new QFutureWatcher<OnboardingResult>(this);
    connect(watcher, &QFutureWatcher<OnboardingResult>::finished,
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
            statusLabel_->setText(tr("Onboarding complete"));
            appendLog(tr("Tenant '%1' onboarded successfully (ID: %2).")
                .arg(wizard_->tenantName(),
                     QString::fromStdString(result.tenantId)));
            appendLog(tr("Admin account '%1' created with TenantAdmin role.")
                .arg(wizard_->adminUsername()));
            progressBar_->setRange(0, 1);
            progressBar_->setValue(1);
            onboardingSuccess_ = true;
            emit wizard_->onboardingCompleted(wizard_->tenantName());
        }

        onboardingComplete_ = true;
        emit completeChanged();
    });

    QFuture<OnboardingResult> future = QtConcurrent::run(
        [clientManager, code, name, type, hostname, description,
         adminUsername, adminPassword, adminEmail]() -> OnboardingResult {

            OnboardingResult result;

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
    if (wizard_->isGleifMode()) {
        appendLog(tr("Tenant details pre-filled from LEI entity: %1")
            .arg(wizard_->rootLeiName()));
    }
    appendLog(tr("Will create admin account '%1'")
        .arg(wizard_->adminUsername()));
}

}
