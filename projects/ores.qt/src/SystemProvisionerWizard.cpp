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
    setPage(Page_AdminAccount, new AdminAccountPage(this));
    setPage(Page_BundleSelection, new BundleSelectionPage(this));
    setPage(Page_Apply, new ApplyProvisioningPage(this));

    setStartId(Page_AdminAccount);
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

    // Confirm password
    confirmPasswordEdit_ = new QLineEdit(this);
    confirmPasswordEdit_->setPlaceholderText(tr("Confirm password"));
    confirmPasswordEdit_->setEchoMode(QLineEdit::Password);
    confirmPasswordEdit_->setMaxLength(128);
    formLayout->addRow(tr("Confirm Password:"), confirmPasswordEdit_);

    layout->addLayout(formLayout);
    layout->addSpacing(20);

    // Validation message label
    validationLabel_ = new QLabel(this);
    validationLabel_->setWordWrap(true);
    validationLabel_->setStyleSheet("QLabel { color: #cc0000; }");
    layout->addWidget(validationLabel_);

    layout->addStretch();

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

    // Bundle selection group
    auto* bundleBox = new QGroupBox(tr("Available Bundles"), this);
    auto* bundleLayout = new QVBoxLayout(bundleBox);

    bundleGroup_ = new QButtonGroup(this);

    const auto bundles = SystemProvisionerWizard::availableBundles();
    for (std::size_t i = 0; i < bundles.size(); ++i) {
        const auto& bundle = bundles[i];
        auto* radio = new QRadioButton(bundle.name, this);
        radio->setProperty("bundleCode", bundle.code);
        radio->setProperty("bundleDescription", bundle.description);
        bundleGroup_->addButton(radio, static_cast<int>(i));
        bundleRadios_.push_back(radio);
        bundleLayout->addWidget(radio);
    }

    layout->addWidget(bundleBox);
    layout->addSpacing(10);

    // Description area
    auto* descBox = new QGroupBox(tr("Description"), this);
    auto* descLayout = new QVBoxLayout(descBox);
    descriptionLabel_ = new QLabel(this);
    descriptionLabel_->setWordWrap(true);
    descriptionLabel_->setMinimumHeight(80);
    descLayout->addWidget(descriptionLabel_);
    layout->addWidget(descBox);

    layout->addStretch();

    // Connect radio buttons to update description
    connect(bundleGroup_, &QButtonGroup::idClicked,
            this, [this](int id) {
        if (id >= 0 && id < static_cast<int>(bundleRadios_.size())) {
            const QString desc = bundleRadios_[id]->property("bundleDescription").toString();
            descriptionLabel_->setText(desc);
        }
        emit completeChanged();
    });
}

void BundleSelectionPage::initializePage() {
    // Select the first bundle by default
    if (!bundleRadios_.empty()) {
        bundleRadios_[0]->setChecked(true);
        descriptionLabel_->setText(
            bundleRadios_[0]->property("bundleDescription").toString());
    }
}

bool BundleSelectionPage::validatePage() {
    QAbstractButton* selected = bundleGroup_->checkedButton();
    if (!selected) {
        QMessageBox::warning(this, tr("Selection Required"),
                             tr("Please select a data bundle to continue."));
        return false;
    }

    const QString bundleCode = selected->property("bundleCode").toString();
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
