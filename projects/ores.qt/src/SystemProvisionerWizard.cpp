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
#include <QTimer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QRegularExpression>
#include <QRegularExpressionValidator>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"

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

    setWindowTitle(tr("System Provisioner"));
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
    setPage(Page_BundleSelection, new BundleSelectionPage(this));
    setPage(Page_Apply, new ApplyProvisioningPage(this));

    setStartId(Page_Welcome);
}

void SystemProvisionerWizard::setAdminCredentials(
    const QString& username, const QString& email, const QString& password) {

    adminUsername_ = username;
    adminEmail_ = email;
    adminPassword_ = password;
}

std::vector<BundleInfo> SystemProvisionerWizard::availableBundles() {
    // Hardcoded from ores.sql/populate/governance/dq_dataset_bundle_populate.sql
    return {
        {
            "solvaris",
            "Solvaris",
            "Synthetic reference data for development and testing. "
            "An isolated fantasy world with fictional currencies, countries, "
            "and entities - ideal for demos and experimentation without "
            "affecting real-world data."
        },
        {
            "base",
            "Base System",
            "Industry-standard reference data (ISO + FpML) for production use. "
            "Includes ISO 3166 countries, ISO 4217 currencies, and FpML "
            "financial reference data. Recommended for production deployments."
        },
        {
            "crypto",
            "Crypto",
            "Base System plus cryptocurrency reference data. "
            "Extends the Base System bundle with additional cryptocurrency "
            "definitions and related assets."
        }
    };
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
           "<li><b>Select Data Bundle</b> - Choose a reference data bundle to "
           "populate the system with initial data.</li>"
           "<li><b>Apply Configuration</b> - The system will be provisioned "
           "with your chosen settings.</li>"
           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    // Info box
    auto* infoBox = new QGroupBox(tr("Note"), this);
    auto* infoLayout = new QVBoxLayout(infoBox);
    auto* infoLabel = new QLabel(
        tr("This wizard only appears during initial system setup. Once "
           "provisioning is complete, you will be able to log in with the "
           "administrator account you create."),
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

bool AdminAccountPage::validatePage() {
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
        passwordMatchLabel_->setText(QString::fromUtf8("\u2713")); // ✓
        passwordMatchLabel_->setStyleSheet("QLabel { color: #228B22; font-weight: bold; font-size: 14pt; }");
    } else {
        // Passwords don't match - red X
        passwordMatchLabel_->setText(QString::fromUtf8("\u2717")); // ✗
        passwordMatchLabel_->setStyleSheet("QLabel { color: #cc0000; font-weight: bold; font-size: 14pt; }");
    }
}

// ============================================================================
// BundleSelectionPage
// ============================================================================

BundleSelectionPage::BundleSelectionPage(SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Select Data Bundle"));
    setSubTitle(tr("Choose a dataset bundle to provision the system with initial "
                   "reference data. You can add more data later."));

    setupUI();
}

void BundleSelectionPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    // Bundle selection
    auto* selectionLayout = new QFormLayout();

    bundleCombo_ = new QComboBox(this);
    const auto bundles = SystemProvisionerWizard::availableBundles();
    for (const auto& bundle : bundles) {
        bundleCombo_->addItem(bundle.name, bundle.code);
    }
    selectionLayout->addRow(tr("Data Bundle:"), bundleCombo_);

    layout->addLayout(selectionLayout);
    layout->addSpacing(20);

    // Description area
    auto* descBox = new QGroupBox(tr("Description"), this);
    auto* descLayout = new QVBoxLayout(descBox);
    descriptionLabel_ = new QLabel(this);
    descriptionLabel_->setWordWrap(true);
    descriptionLabel_->setMinimumHeight(100);
    descLayout->addWidget(descriptionLabel_);
    layout->addWidget(descBox);

    layout->addStretch();

    // Connect combo box to update description
    connect(bundleCombo_, &QComboBox::currentIndexChanged,
            this, &BundleSelectionPage::onBundleChanged);
}

void BundleSelectionPage::onBundleChanged(int index) {
    const auto bundles = SystemProvisionerWizard::availableBundles();
    if (index >= 0 && index < static_cast<int>(bundles.size())) {
        descriptionLabel_->setText(bundles[index].description);
    }
    emit completeChanged();
}

void BundleSelectionPage::initializePage() {
    // Change Next button to "Provision" to indicate action
    wizard()->setButtonText(QWizard::NextButton, tr("Provision"));

    // Select the first bundle by default and show its description
    if (bundleCombo_->count() > 0) {
        bundleCombo_->setCurrentIndex(0);
        onBundleChanged(0);
    }
}

bool BundleSelectionPage::validatePage() {
    if (bundleCombo_->currentIndex() < 0) {
        QMessageBox::warning(this, tr("Selection Required"),
                             tr("Please select a data bundle to continue."));
        return false;
    }

    const QString bundleCode = bundleCombo_->currentData().toString();
    wizard_->setSelectedBundleCode(bundleCode);

    return true;
}

// ============================================================================
// ApplyProvisioningPage
// ============================================================================

ApplyProvisioningPage::ApplyProvisioningPage(SystemProvisionerWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Applying Provisioning"));
    setSubTitle(tr("Please wait while the system is being provisioned."));
    setFinalPage(true);

    auto* layout = new QVBoxLayout(this);

    // Status label
    statusLabel_ = new QLabel(tr("Preparing to provision..."), this);
    statusLabel_->setAlignment(Qt::AlignCenter);
    statusLabel_->setStyleSheet("font-size: 14px; font-weight: bold;");
    layout->addWidget(statusLabel_);

    layout->addSpacing(20);

    // Progress bar
    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);  // Indeterminate initially
    progressBar_->setMinimumWidth(400);
    layout->addWidget(progressBar_, 0, Qt::AlignCenter);

    layout->addSpacing(20);

    // Log output
    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setMinimumHeight(200);
    logOutput_->setStyleSheet(
        "QTextEdit { background-color: #1e1e1e; color: #d4d4d4; "
        "font-family: monospace; font-size: 11px; }");
    layout->addWidget(logOutput_, 1);

    // Hide navigation buttons during provisioning
    setCommitPage(true);

    // Register ProvisioningResult for cross-thread signal/slot
    qRegisterMetaType<ProvisioningResult>("ProvisioningResult");
}

void ApplyProvisioningPage::initializePage() {
    provisioningComplete_ = false;
    provisioningSuccess_ = false;
    logOutput_->clear();
    progressBar_->setRange(0, 0);  // Indeterminate

    // Start provisioning after a short delay
    QTimer::singleShot(100, this, &ApplyProvisioningPage::startProvisioning);
}

bool ApplyProvisioningPage::isComplete() const {
    return provisioningComplete_;
}

void ApplyProvisioningPage::setStatus(const QString& status) {
    statusLabel_->setText(status);
}

void ApplyProvisioningPage::appendLog(const QString& message) {
    logOutput_->append(message);
    // Scroll to bottom
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void ApplyProvisioningPage::startProvisioning() {
    BOOST_LOG_SEV(lg(), info) << "Starting system provisioning (async)";

    setStatus(tr("Provisioning system..."));
    appendLog(tr("Starting provisioning..."));

    // Capture values needed for background thread (avoid accessing wizard_ from thread)
    const std::string username = wizard_->adminUsername().toStdString();
    const std::string password = wizard_->adminPassword().toStdString();
    const std::string email = wizard_->adminEmail().toStdString();
    const QString bundleCode = wizard_->selectedBundleCode();
    ClientManager* clientManager = wizard_->clientManager();

    // Perform provisioning asynchronously
    auto* watcher = new QFutureWatcher<ProvisioningResult>(this);
    connect(watcher, &QFutureWatcher<ProvisioningResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        onProvisioningResult(result);
    });

    QFuture<ProvisioningResult> future = QtConcurrent::run(
        [clientManager, username, password, email, bundleCode]() -> ProvisioningResult {
            ProvisioningResult result;

            // Step 1: Create administrator account
            result.log_messages.append(
                QString("[1/3] Creating administrator account: %1").arg(QString::fromStdString(username)));

            iam::messaging::create_initial_admin_request adminRequest;
            adminRequest.username = username;
            adminRequest.password = password;
            adminRequest.email = email;

            auto adminResult = clientManager->process_request(std::move(adminRequest));

            if (!adminResult) {
                result.success = false;
                result.error_message = "Failed to communicate with server.";
                result.log_messages.append("ERROR: Failed to communicate with server.");
                return result;
            }

            if (!adminResult->success) {
                result.success = false;
                result.error_message = QString::fromStdString(adminResult->error_message);
                result.log_messages.append(QString("ERROR: %1").arg(result.error_message));
                return result;
            }

            result.admin_account_id = adminResult->account_id;
            result.log_messages.append(QString("SUCCESS: Administrator account created (ID: %1)")
                .arg(QString::fromStdString(boost::uuids::to_string(adminResult->account_id))));

            // Step 2: Apply selected bundle
            result.log_messages.append(QString("[2/3] Applying data bundle: %1").arg(bundleCode));

            // TODO: Server-side bundle application is not yet implemented.
            // When implemented, this will send an apply_bundle_request to the server.
            result.log_messages.append("NOTE: Bundle application server message not yet implemented.");
            result.log_messages.append(QString("TODO: Implement apply_bundle_request for bundle '%1'").arg(bundleCode));

            // Step 3: Finalize
            result.log_messages.append("[3/3] Finalizing provisioning...");
            result.log_messages.append("SUCCESS: System provisioning completed.");
            result.log_messages.append("");
            result.log_messages.append("You can now log in with the administrator account.");

            result.success = true;
            return result;
        }
    );

    watcher->setFuture(future);
}

ProvisioningResult ApplyProvisioningPage::performProvisioning() {
    // This method is now only used as a helper if needed
    // The actual work is done in the lambda passed to QtConcurrent::run
    return {};
}

void ApplyProvisioningPage::onProvisioningResult(const ProvisioningResult& result) {
    BOOST_LOG_SEV(lg(), debug) << "Provisioning result received";

    // Display all log messages
    for (const QString& msg : result.log_messages) {
        appendLog(msg);
    }

    // Set progress bar to complete
    progressBar_->setRange(0, 3);
    progressBar_->setValue(3);

    if (result.success) {
        wizard_->setAdminAccountId(result.admin_account_id);
        setStatus(tr("Provisioning completed successfully!"));
        provisioningComplete_ = true;
        provisioningSuccess_ = true;
        emit completeChanged();

        // Show next steps message
        appendLog("");
        appendLog(tr("=== Setup Complete ==="));
        appendLog(tr("You can now log in to the system using the administrator account:"));
        appendLog(tr("  Username: %1").arg(wizard_->adminUsername()));
        appendLog("");
        appendLog(tr("Click 'Finish' to close this wizard and return to the login screen."));

        BOOST_LOG_SEV(lg(), info) << "System provisioning completed successfully";
        emit wizard_->provisioningCompleted(wizard_->adminUsername());
    } else {
        BOOST_LOG_SEV(lg(), error) << "Provisioning failed: " << result.error_message.toStdString();
        setStatus(tr("Provisioning failed!"));
        provisioningComplete_ = true;
        provisioningSuccess_ = false;
        emit completeChanged();
        emit wizard_->provisioningFailed(result.error_message);
    }
}

}
