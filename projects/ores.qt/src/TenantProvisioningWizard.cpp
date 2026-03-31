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
#include "ores.qt/TenantProvisioningWizard.hpp"
#include "ores.qt/ClientDatasetBundleModel.hpp"
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LeiEntityPicker.hpp"
#include "ores.qt/WidgetUtils.hpp"

#include <array>
#include <chrono>
#include <QFormLayout>
#include <QGridLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QListWidgetItem>
#include <QRegularExpressionValidator>
#include <QSizePolicy>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.synthetic.api/messaging/generate_organisation_protocol.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace reason = ores::database::domain::change_reason_constants;

// ============================================================================
// TenantProvisioningWizard
// ============================================================================

TenantProvisioningWizard::TenantProvisioningWizard(
    ClientManager* clientManager,
    QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager) {

    setWindowTitle(tr("New Tenant Provisioner"));
    setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::BuildingSkyscraper, IconUtils::DefaultIconColor));
    setMinimumSize(900, 700);
    resize(900, 700);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();

    // Clear bootstrap flag on cancel/reject too
    connect(this, &QWizard::rejected, this, [this]() {
        clearBootstrapFlag();
    });
}

void TenantProvisioningWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Welcome,             new ProvisioningWelcomePage(this));
    setPage(Page_BundleSelection,     new BundleSelectionPage(this));
    setPage(Page_BundleInstall,       new BundleInstallPage(this));
    setPage(Page_DataSourceSelection, new TenantDataSourceSelectionPage(this));
    setPage(Page_PartySetup,          new TenantPartySetupPage(this));
    setPage(Page_PartyOrganisation,   new TenantPartyOrganisationPage(this));
    setPage(Page_Summary,             new TenantApplyAndSummaryPage(this));

    setStartId(Page_Welcome);
}

void TenantProvisioningWizard::clearBootstrapFlag() {
    BOOST_LOG_SEV(lg(), info) << "Clearing tenant bootstrap mode flag";

    variability::domain::system_setting setting;
    setting.name = "system.bootstrap_mode";
    setting.value = "false";
    setting.data_type = "boolean";
    setting.description = "Bootstrap mode disabled after tenant setup";
    setting.modified_by = clientManager_->currentUsername();
    setting.change_reason_code = std::string(reason::codes::new_record);
    setting.change_commentary = "Tenant setup wizard completed";
    variability::messaging::save_setting_request req;
    req.data = std::move(setting);

    auto result = clientManager_->process_authenticated_request(std::move(req));
    if (!result) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to clear bootstrap flag: "
                                  << "no response from server";
    } else if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to clear bootstrap flag: "
                                  << (result->message.empty() ? "Unknown error" : result->message);
    } else {
        BOOST_LOG_SEV(lg(), info) << "Bootstrap flag cleared successfully";
    }
}

// ============================================================================
// ProvisioningWelcomePage
// ============================================================================

ProvisioningWelcomePage::ProvisioningWelcomePage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Welcome"));
    setupUI();
}

void ProvisioningWelcomePage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* welcomeLabel = new QLabel(
        tr("Your tenant is new and needs initial setup."), this);
    welcomeLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    welcomeLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(welcomeLabel);

    layout->addSpacing(10);

    auto* descLabel = new QLabel(this);
    descLabel->setWordWrap(true);
    descLabel->setText(
        tr("This wizard will help you set up your tenant with essential "
           "reference data, parties, and organisational structure.\n\n"
           "The setup process includes:"));
    layout->addWidget(descLabel);

    auto* stepsLabel = new QLabel(this);
    stepsLabel->setWordWrap(true);
    stepsLabel->setTextFormat(Qt::RichText);
    stepsLabel->setText(
        tr("<ol>"
           "<li><b>Select Catalogue</b> - Choose a pre-configured set of "
           "reference data (currencies, countries, etc.).</li>"
           "<li><b>Publish Catalogue</b> - Publish the selected data to your "
           "tenant.</li>"
           "<li><b>Party Structure</b> - Set up your organisation's party "
           "hierarchy from the GLEIF LEI registry or from synthetic data.</li>"
           "<li><b>Organisation Setup</b> - Publish parties, counterparties, "
           "and organisational structure.</li>"
           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    auto* noteBox = new QGroupBox(tr("Note"), this);
    auto* noteLayout = new QVBoxLayout(noteBox);
    auto* noteLabel = new QLabel(
        tr("You can skip this setup by clicking Cancel. The wizard will not "
           "appear again, and you can set up reference data and parties "
           "manually using the Data Librarian and Parties window."),
        this);
    noteLabel->setWordWrap(true);
    noteLayout->addWidget(noteLabel);
    layout->addWidget(noteBox);
}

// ============================================================================
// BundleSelectionPage
// ============================================================================

BundleSelectionPage::BundleSelectionPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Select Catalogue"));
    setSubTitle(tr("Choose a catalogue of reference data to publish to your "
                   "tenant. Each catalogue contains a pre-configured set of "
                   "currencies, countries, and other reference data."));

    setupUI();
}

void BundleSelectionPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    bundleModel_ = new ClientDatasetBundleModel(
        wizard_->clientManager(), this);

    auto* comboLabel = new QLabel(tr("Catalogue:"), this);
    layout->addWidget(comboLabel);

    bundleCombo_ = new QComboBox(this);
    bundleCombo_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    layout->addWidget(bundleCombo_);

    layout->addSpacing(12);

    descriptionLabel_ = new QLabel(this);
    descriptionLabel_->setWordWrap(true);
    descriptionLabel_->setTextFormat(Qt::PlainText);
    descriptionLabel_->setFrameShape(QFrame::StyledPanel);
    descriptionLabel_->setContentsMargins(8, 8, 8, 8);
    descriptionLabel_->setMinimumHeight(60);
    layout->addWidget(descriptionLabel_);

    layout->addStretch();

    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);

    connect(bundleCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &BundleSelectionPage::onBundleChanged);

    // Populate combo when data arrives
    connect(bundleModel_, &ClientDatasetBundleModel::dataLoaded,
            this, [this]() {
        bundleCombo_->clear();
        const int count = bundleModel_->rowCount();
        for (int i = 0; i < count; ++i) {
            const auto* bundle = bundleModel_->getBundle(i);
            if (bundle) {
                bundleCombo_->addItem(
                    QString::fromStdString(bundle->name), i);
            }
        }
        if (count > 0) {
            bundleCombo_->setCurrentIndex(0);
            statusLabel_->clear();
        } else {
            statusLabel_->setText(tr("No catalogues available."));
        }
        emit completeChanged();
    });

    connect(bundleModel_, &ClientDatasetBundleModel::loadError,
            this, [this](const QString& msg) {
        statusLabel_->setText(tr("Failed to load catalogues: %1").arg(msg));
    });

    WidgetUtils::setupComboBoxes(this);
}

void BundleSelectionPage::onBundleChanged(int index) {
    if (index < 0) {
        descriptionLabel_->clear();
        return;
    }

    const int row = bundleCombo_->itemData(index).toInt();
    const auto* bundle = bundleModel_->getBundle(row);
    if (bundle) {
        descriptionLabel_->setText(
            QString::fromStdString(bundle->description));
    } else {
        descriptionLabel_->clear();
    }
    emit completeChanged();
}

void BundleSelectionPage::initializePage() {
    bundleCombo_->clear();
    descriptionLabel_->clear();
    bundleModel_->refresh();
    statusLabel_->setText(tr("Loading available catalogues..."));
}

bool BundleSelectionPage::isComplete() const {
    return bundleCombo_->currentIndex() >= 0;
}

bool BundleSelectionPage::validatePage() {
    const int index = bundleCombo_->currentIndex();
    if (index < 0) {
        statusLabel_->setText(tr("Please select a catalogue to continue."));
        return false;
    }

    const int row = bundleCombo_->itemData(index).toInt();
    const auto* bundle = bundleModel_->getBundle(row);
    if (!bundle) {
        statusLabel_->setText(tr("Invalid selection."));
        return false;
    }

    wizard_->setSelectedBundleCode(QString::fromStdString(bundle->code));
    wizard_->setSelectedBundleName(QString::fromStdString(bundle->name));
    return true;
}

// ============================================================================
// BundleInstallPage
// ============================================================================

BundleInstallPage::BundleInstallPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Publishing Catalogue"));
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

bool BundleInstallPage::isComplete() const {
    return publishComplete_;
}

void BundleInstallPage::initializePage() {
    publishComplete_ = false;
    publishSuccess_ = false;
    logOutput_->clear();
    statusLabel_->setText(tr("Publishing catalogue '%1'...").arg(
        wizard_->selectedBundleName()));
    progressBar_->setRange(0, 0);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");

    startPublish();
}

void BundleInstallPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void BundleInstallPage::startPublish() {
    const std::string bundleCode = wizard_->selectedBundleCode().toStdString();
    const std::string publishedBy = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Publishing bundle: " << bundleCode;

    using ResponseType = dq::messaging::publish_bundle_response;

    auto* watcher = new QFutureWatcher<std::optional<ResponseType>>(this);
    connect(watcher, &QFutureWatcher<std::optional<ResponseType>>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "Bundle publication: no server response";
            statusLabel_->setText(tr("Publication failed!"));
            appendLog(tr("ERROR: Failed to communicate with server."));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");
            publishSuccess_ = false;
        } else if (!result->success) {
            BOOST_LOG_SEV(lg(), error) << "Bundle publication failed: "
                                       << result->error_message;
            statusLabel_->setText(tr("Publication failed!"));
            appendLog(tr("ERROR: %1").arg(
                QString::fromStdString(result->error_message)));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");
            publishSuccess_ = false;
        } else {
            BOOST_LOG_SEV(lg(), info) << "Bundle publication succeeded: "
                << result->datasets_succeeded << " datasets";
            statusLabel_->setText(tr("Publication completed successfully!"));
            appendLog(tr("Published %1 datasets (%2 records inserted, %3 updated).")
                .arg(result->datasets_succeeded)
                .arg(result->total_records_inserted)
                .arg(result->total_records_updated));
            publishSuccess_ = true;
        }

        publishComplete_ = true;
        emit completeChanged();
    });

    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, bundleCode, publishedBy]() -> std::optional<ResponseType> {

            dq::messaging::publish_bundle_request request;
            request.bundle_code = bundleCode;
            request.mode = dq::domain::publication_mode::upsert;
            request.published_by = publishedBy;
            request.atomic = true;

            auto result = clientManager->process_authenticated_request(
                std::move(request));

            if (!result) {
                return std::nullopt;
            }
            return *result;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("Publishing catalogue '%1'...")
        .arg(wizard_->selectedBundleName()));
}

// ============================================================================
// TenantDataSourceSelectionPage
// ============================================================================

TenantDataSourceSelectionPage::TenantDataSourceSelectionPage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Choose Data Source"));
    setSubTitle(tr("Select how to populate parties, counterparties, and "
                   "organisational structure for your tenant."));

    setupUI();
}

void TenantDataSourceSelectionPage::setupUI() {
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(12);

    gleifRadio_ = new QRadioButton(
        tr("GLEIF Registry - Search the LEI registry for your organisation"),
        this);
    gleifRadio_->setChecked(true);
    layout->addWidget(gleifRadio_);

    syntheticRadio_ = new QRadioButton(
        tr("Generate Synthetic Data - Create realistic generated data"),
        this);
    layout->addWidget(syntheticRadio_);

    layout->addSpacing(8);

    // Synthetic options group (shown only when synthetic is selected)
    syntheticOptions_ = new QWidget(this);
    auto* optLayout = new QGridLayout(syntheticOptions_);
    optLayout->setContentsMargins(20, 0, 0, 0);
    optLayout->setColumnStretch(1, 1);

    int row = 0;

    // --- Entity counts -------------------------------------------------------
    optLayout->addWidget(new QLabel(tr("Country:"), this), row, 0);
    countryCombo_ = new QComboBox(this);
    countryCombo_->addItem(tr("United Kingdom (GB)"), "GB");
    countryCombo_->addItem(tr("United States (US)"), "US");
    optLayout->addWidget(countryCombo_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Party count:"), this), row, 0);
    partyCountSpin_ = new QSpinBox(this);
    partyCountSpin_->setRange(1, 100);
    partyCountSpin_->setValue(5);
    optLayout->addWidget(partyCountSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Counterparty count:"), this), row, 0);
    counterpartyCountSpin_ = new QSpinBox(this);
    counterpartyCountSpin_->setRange(1, 200);
    counterpartyCountSpin_->setValue(10);
    optLayout->addWidget(counterpartyCountSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Portfolio leaf count:"), this), row, 0);
    portfolioLeafCountSpin_ = new QSpinBox(this);
    portfolioLeafCountSpin_->setRange(1, 100);
    portfolioLeafCountSpin_->setValue(8);
    optLayout->addWidget(portfolioLeafCountSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Books per portfolio:"), this), row, 0);
    booksPerPortfolioSpin_ = new QSpinBox(this);
    booksPerPortfolioSpin_->setRange(1, 20);
    booksPerPortfolioSpin_->setValue(2);
    optLayout->addWidget(booksPerPortfolioSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Business unit count:"), this), row, 0);
    businessUnitCountSpin_ = new QSpinBox(this);
    businessUnitCountSpin_->setRange(1, 100);
    businessUnitCountSpin_->setValue(10);
    optLayout->addWidget(businessUnitCountSpin_, row, 1);
    row++;

    // --- Hierarchy depths ----------------------------------------------------
    auto* depthSep = new QLabel(tr("<b>Hierarchy depths</b>"), this);
    depthSep->setContentsMargins(0, 6, 0, 2);
    optLayout->addWidget(depthSep, row, 0, 1, 2);
    row++;

    optLayout->addWidget(new QLabel(tr("Party max depth:"), this), row, 0);
    partyMaxDepthSpin_ = new QSpinBox(this);
    partyMaxDepthSpin_->setRange(1, 10);
    partyMaxDepthSpin_->setValue(3);
    optLayout->addWidget(partyMaxDepthSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Counterparty max depth:"), this), row, 0);
    counterpartyMaxDepthSpin_ = new QSpinBox(this);
    counterpartyMaxDepthSpin_->setRange(1, 10);
    counterpartyMaxDepthSpin_->setValue(3);
    optLayout->addWidget(counterpartyMaxDepthSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Portfolio max depth:"), this), row, 0);
    portfolioMaxDepthSpin_ = new QSpinBox(this);
    portfolioMaxDepthSpin_->setRange(1, 10);
    portfolioMaxDepthSpin_->setValue(4);
    optLayout->addWidget(portfolioMaxDepthSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Business unit max depth:"), this), row, 0);
    businessUnitMaxDepthSpin_ = new QSpinBox(this);
    businessUnitMaxDepthSpin_->setRange(1, 5);
    businessUnitMaxDepthSpin_->setValue(2);
    optLayout->addWidget(businessUnitMaxDepthSpin_, row, 1);
    row++;

    // --- Contact and identifier options --------------------------------------
    auto* contactSep = new QLabel(tr("<b>Contacts &amp; identifiers</b>"), this);
    contactSep->setContentsMargins(0, 6, 0, 2);
    optLayout->addWidget(contactSep, row, 0, 1, 2);
    row++;

    optLayout->addWidget(new QLabel(tr("Contacts per party:"), this), row, 0);
    contactsPerPartySpin_ = new QSpinBox(this);
    contactsPerPartySpin_->setRange(0, 10);
    contactsPerPartySpin_->setValue(2);
    optLayout->addWidget(contactsPerPartySpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Contacts per counterparty:"), this), row, 0);
    contactsPerCounterpartySpin_ = new QSpinBox(this);
    contactsPerCounterpartySpin_->setRange(0, 10);
    contactsPerCounterpartySpin_->setValue(1);
    optLayout->addWidget(contactsPerCounterpartySpin_, row, 1);
    row++;

    generateAddressesCheck_ = new QCheckBox(tr("Generate addresses"), this);
    generateAddressesCheck_->setChecked(true);
    optLayout->addWidget(generateAddressesCheck_, row, 0, 1, 2);
    row++;

    generateIdentifiersCheck_ = new QCheckBox(tr("Generate identifiers (LEI, BIC)"), this);
    generateIdentifiersCheck_->setChecked(true);
    optLayout->addWidget(generateIdentifiersCheck_, row, 0, 1, 2);
    row++;

    // --- Reproducibility -----------------------------------------------------
    auto* seedSep = new QLabel(tr("<b>Reproducibility</b>"), this);
    seedSep->setContentsMargins(0, 6, 0, 2);
    optLayout->addWidget(seedSep, row, 0, 1, 2);
    row++;

    optLayout->addWidget(new QLabel(tr("Seed (optional):"), this), row, 0);
    seedEdit_ = new QLineEdit(this);
    seedEdit_->setPlaceholderText(tr("Leave blank for a random seed"));
    seedEdit_->setValidator(new QRegularExpressionValidator(
        QRegularExpression("[0-9]*"), seedEdit_));
    optLayout->addWidget(seedEdit_, row, 1);

    syntheticOptions_->setVisible(false);
    layout->addWidget(syntheticOptions_);

    layout->addStretch();

    connect(gleifRadio_, &QRadioButton::toggled,
            this, &TenantDataSourceSelectionPage::onModeChanged);
    connect(syntheticRadio_, &QRadioButton::toggled,
            this, &TenantDataSourceSelectionPage::onModeChanged);

    WidgetUtils::setupComboBoxes(this);
}

void TenantDataSourceSelectionPage::onModeChanged() {
    syntheticOptions_->setVisible(syntheticRadio_->isChecked());
}

bool TenantDataSourceSelectionPage::validatePage() {
    if (syntheticRadio_->isChecked()) {
        wizard_->setDataSourceMode(TenantProvisioningWizard::DataSourceMode::synthetic);
        wizard_->setSyntheticCountry(countryCombo_->currentData().toString());
        wizard_->setSyntheticPartyCount(partyCountSpin_->value());
        wizard_->setSyntheticPartyMaxDepth(partyMaxDepthSpin_->value());
        wizard_->setSyntheticCounterpartyCount(counterpartyCountSpin_->value());
        wizard_->setSyntheticCounterpartyMaxDepth(counterpartyMaxDepthSpin_->value());
        wizard_->setSyntheticPortfolioLeafCount(portfolioLeafCountSpin_->value());
        wizard_->setSyntheticPortfolioMaxDepth(portfolioMaxDepthSpin_->value());
        wizard_->setSyntheticBooksPerPortfolio(booksPerPortfolioSpin_->value());
        wizard_->setSyntheticBusinessUnitCount(businessUnitCountSpin_->value());
        wizard_->setSyntheticBusinessUnitMaxDepth(businessUnitMaxDepthSpin_->value());
        wizard_->setSyntheticGenerateAddresses(generateAddressesCheck_->isChecked());
        wizard_->setSyntheticContactsPerParty(contactsPerPartySpin_->value());
        wizard_->setSyntheticContactsPerCounterparty(contactsPerCounterpartySpin_->value());
        wizard_->setSyntheticGenerateIdentifiers(generateIdentifiersCheck_->isChecked());

        const auto seedText = seedEdit_->text().trimmed();
        if (seedText.isEmpty()) {
            wizard_->setSyntheticSeed(std::nullopt);
        } else {
            bool ok = false;
            const auto seed = seedText.toULongLong(&ok);
            wizard_->setSyntheticSeed(ok ? std::optional<std::uint64_t>{seed}
                                         : std::nullopt);
        }
    } else {
        wizard_->setDataSourceMode(TenantProvisioningWizard::DataSourceMode::gleif);
    }
    return true;
}

int TenantDataSourceSelectionPage::nextId() const {
    if (syntheticRadio_->isChecked())
        return TenantProvisioningWizard::Page_PartyOrganisation;
    return TenantProvisioningWizard::Page_PartySetup;
}

// ============================================================================
// TenantPartySetupPage
// ============================================================================

TenantPartySetupPage::TenantPartySetupPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Party Setup (Optional)"));
    setSubTitle(tr("Optionally select a root LEI entity to configure your "
                   "organisation's party hierarchy."));

    setupUI();
}

void TenantPartySetupPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    instructionLabel_ = new QLabel(this);
    instructionLabel_->setWordWrap(true);
    instructionLabel_->setText(
        tr("Search the GLEIF LEI registry for your organisation. The selected "
           "entity will become the root of your party hierarchy. All child "
           "entities in the GLEIF registry will be created automatically.\n\n"
           "This step is optional - you can skip it and set up parties "
           "manually later from the Parties window."));
    layout->addWidget(instructionLabel_);

    layout->addSpacing(10);

    // Dataset size selection
    auto* sizeLayout = new QHBoxLayout();
    sizeLayout->addWidget(new QLabel(tr("LEI dataset size:"), this));
    datasetSizeCombo_ = new QComboBox(this);
    datasetSizeCombo_->addItem(tr("Large (~15,000 entities)"), "large");
    datasetSizeCombo_->addItem(tr("Small (~6,000 entities)"), "small");
    datasetSizeCombo_->setCurrentIndex(0);
    sizeLayout->addWidget(datasetSizeCombo_);
    sizeLayout->addStretch();
    layout->addLayout(sizeLayout);

    layout->addSpacing(10);

    leiPicker_ = new LeiEntityPicker(wizard_->clientManager(), this);
    layout->addWidget(leiPicker_);

    WidgetUtils::setupComboBoxes(this);
}

void TenantPartySetupPage::initializePage() {
    if (!leiLoaded_) {
        leiPicker_->load();
        leiLoaded_ = true;
    }
}

bool TenantPartySetupPage::validatePage() {
    // Optional page - always allow advancing
    if (leiPicker_->hasSelection()) {
        wizard_->setRootLei(leiPicker_->selectedLei());
        wizard_->setRootLeiName(leiPicker_->selectedName());
    }
    wizard_->setLeiDatasetSize(datasetSizeCombo_->currentData().toString());
    return true;
}

// ============================================================================
// TenantPartyOrganisationPage
// ============================================================================

TenantPartyOrganisationPage::TenantPartyOrganisationPage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Organisation Setup"));
    setSubTitle(tr("Setting up your organisation's structure."));
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

bool TenantPartyOrganisationPage::isComplete() const {
    return publishComplete_;
}

void TenantPartyOrganisationPage::initializePage() {
    publishComplete_ = false;
    publishSuccess_ = false;
    logOutput_->clear();
    progressBar_->setRange(0, 0);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");

    const bool isSynthetic = wizard_->dataSourceMode() ==
        TenantProvisioningWizard::DataSourceMode::synthetic;

    if (isSynthetic) {
        setSubTitle(tr("Generating synthetic parties, counterparties, "
                       "business units, portfolios, and trading books."));
        statusLabel_->setText(tr("Generating synthetic organisation data..."));
    } else if (!wizard_->rootLei().isEmpty()) {
        setSubTitle(tr("Publishing GLEIF party hierarchy, business units, "
                       "portfolios, and trading books for your organisation."));
        statusLabel_->setText(tr("Publishing organisation data with GLEIF "
                                 "parties..."));
    } else {
        setSubTitle(tr("Publishing business units, portfolios, and trading "
                       "books for your organisation."));
        statusLabel_->setText(tr("Publishing organisation data..."));
    }

    startPublish();
}

void TenantPartyOrganisationPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void TenantPartyOrganisationPage::startPublish() {
    const bool isSynthetic = wizard_->dataSourceMode() ==
        TenantProvisioningWizard::DataSourceMode::synthetic;

    if (isSynthetic)
        startSyntheticGeneration();
    else
        startBundlePublish();
}

void TenantPartyOrganisationPage::startBundlePublish() {
    const std::string publishedBy = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();
    const std::string rootLei = wizard_->rootLei().toStdString();
    const bool hasLei = !rootLei.empty();

    BOOST_LOG_SEV(lg(), info) << "Publishing organisation data"
                              << (hasLei ? " with root LEI: " : "")
                              << rootLei;

    // Build LEI params for re-publishing the base bundle with opt-in datasets
    std::string leiParamsJson = "{}";
    if (hasLei) {
        dq::messaging::publish_bundle_params leiParams;
        leiParams.lei_parties = dq::messaging::lei_parties_params{rootLei};
        const std::string datasetSize = wizard_->leiDatasetSize().toStdString();
        const std::string size = datasetSize.empty() ? "small" : datasetSize;
        leiParams.opted_in_datasets.push_back("gleif.lei_parties." + size);
        leiParams.opted_in_datasets.push_back("gleif.lei_counterparties." + size);
        leiParamsJson = dq::messaging::build_params_json(leiParams);
    }

    struct PublishResult {
        bool success = false;
        std::string error_message;
        int datasets_succeeded = 0;
        int total_records_inserted = 0;
        int total_records_updated = 0;
        int parties_linked = 0;
    };

    auto* watcher = new QFutureWatcher<PublishResult>(this);
    connect(watcher, &QFutureWatcher<PublishResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error)
                << "Organisation publication failed: " << result.error_message;
            statusLabel_->setText(tr("Organisation setup failed!"));
            appendLog(tr("ERROR: %1").arg(
                QString::fromStdString(result.error_message)));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");
            publishSuccess_ = false;
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Organisation setup succeeded: "
                << result.datasets_succeeded << " datasets, "
                << result.parties_linked << " parties linked";
            statusLabel_->setText(tr("Organisation setup complete!"));
            appendLog(tr("Published %1 datasets (%2 records inserted, %3 updated).")
                .arg(result.datasets_succeeded)
                .arg(result.total_records_inserted)
                .arg(result.total_records_updated));
            if (result.parties_linked > 0) {
                appendLog(tr("Tenant admin associated with %1 parties.")
                    .arg(result.parties_linked));
            }
            wizard_->setPartiesLinkedCount(result.parties_linked);
            publishSuccess_ = true;
        }

        publishComplete_ = true;
        emit completeChanged();
    });

    QFuture<PublishResult> future = QtConcurrent::run(
        [clientManager, publishedBy, leiParamsJson, hasLei, rootLei,
         rootLeiName = wizard_->rootLeiName().toStdString(),
         leiDatasetSize = wizard_->leiDatasetSize().toStdString()]()
            -> PublishResult {

            PublishResult result;

            // Step 1: Publish LEI parties and counterparties from base bundle
            if (hasLei) {
                dq::messaging::publish_bundle_request leiRequest;
                leiRequest.bundle_code = "standard";
                leiRequest.mode = dq::domain::publication_mode::upsert;
                leiRequest.published_by = publishedBy;
                leiRequest.atomic = true;
                leiRequest.params_json = leiParamsJson;

                auto leiResult = clientManager->process_authenticated_request(
                    std::move(leiRequest), std::chrono::minutes(5));

                if (!leiResult) {
                    result.error_message = "Failed to communicate with server "
                        "(GLEIF parties)";
                    return result;
                }
                if (!leiResult->success) {
                    result.error_message = leiResult->error_message;
                    return result;
                }
            }

            // Step 2: Publish organisation bundle (business units, portfolios, books)
            dq::messaging::publish_bundle_request orgRequest;
            orgRequest.bundle_code = "organisation";
            orgRequest.mode = dq::domain::publication_mode::upsert;
            orgRequest.published_by = publishedBy;
            orgRequest.atomic = true;

            auto orgResult = clientManager->process_authenticated_request(
                std::move(orgRequest), std::chrono::minutes(5));

            if (!orgResult) {
                result.error_message = "Failed to communicate with server "
                    "(organisation bundle)";
                return result;
            }
            if (!orgResult->success) {
                result.error_message = orgResult->error_message;
                return result;
            }

            result.datasets_succeeded   = orgResult->datasets_succeeded;
            result.total_records_inserted = orgResult->total_records_inserted;
            result.total_records_updated  = orgResult->total_records_updated;

            // Step 3: Associate tenant admin with all Operational parties
            const auto accountId = clientManager->accountId();
            if (!accountId) {
                // No account ID available - skip linking (non-fatal)
                result.success = true;
                return result;
            }

            refdata::messaging::get_parties_request partyReq;
            partyReq.offset = 0;
            partyReq.limit = 1000;
            auto partyResult = clientManager->process_authenticated_request(
                std::move(partyReq));

            if (!partyResult) {
                // Non-fatal: parties were created, just couldn't link admin
                result.success = true;
                return result;
            }

            iam::messaging::save_account_party_request apReq;
            for (const auto& party : partyResult->parties) {
                if (party.party_category != "Operational")
                    continue;
                iam::domain::account_party ap;
                ap.account_id = *accountId;
                ap.party_id = party.id;
                ap.tenant_id = party.tenant_id;
                ap.modified_by = publishedBy;
                ap.performed_by = publishedBy;
                ap.change_reason_code =
                    std::string(reason::codes::new_record);
                ap.change_commentary =
                    "Tenant provisioning: tenant admin associated with party";
                apReq.account_parties.push_back(std::move(ap));
            }

            if (!apReq.account_parties.empty()) {
                auto apResult = clientManager->process_authenticated_request(
                    std::move(apReq));
                if (apResult && apResult->success) {
                    result.parties_linked =
                        static_cast<int>(apReq.account_parties.size());
                }
            }

            result.success = true;
            return result;
        }
    );

    watcher->setFuture(future);

    if (hasLei) {
        appendLog(tr("[1/2] Publishing GLEIF LEI parties and counterparties "
                      "(root: %1, dataset: %2)...")
            .arg(wizard_->rootLeiName(), wizard_->leiDatasetSize()));
        appendLog(tr("[2/2] Publishing organisation structure (business units, "
                      "portfolios, books)..."));
    } else {
        appendLog(tr("Publishing organisation bundle (business units, portfolios, "
                      "books)..."));
    }
}

void TenantPartyOrganisationPage::startSyntheticGeneration() {
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Generating synthetic organisation data";

    struct PublishResult {
        bool success = false;
        std::string error_message;
        int parties_generated = 0;
        int parties_linked = 0;
    };

    auto* watcher = new QFutureWatcher<PublishResult>(this);
    connect(watcher, &QFutureWatcher<PublishResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error)
                << "Synthetic generation failed: " << result.error_message;
            statusLabel_->setText(tr("Synthetic generation failed!"));
            appendLog(tr("ERROR: %1").arg(
                QString::fromStdString(result.error_message)));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");
            publishSuccess_ = false;
        } else {
            BOOST_LOG_SEV(lg(), info) << "Synthetic generation succeeded";
            statusLabel_->setText(tr("Organisation setup complete!"));
            appendLog(tr("Synthetic organisation data generated (%1 parties).")
                .arg(result.parties_generated));
            if (result.parties_linked > 0) {
                appendLog(tr("Tenant admin associated with %1 parties.")
                    .arg(result.parties_linked));
            }
            wizard_->setPartiesLinkedCount(result.parties_linked);
            publishSuccess_ = true;
        }

        publishComplete_ = true;
        emit completeChanged();
    });

    using SyntheticResponseType =
        synthetic::messaging::generate_organisation_response;

    QFuture<PublishResult> future = QtConcurrent::run(
        [clientManager,
         publishedBy = wizard_->clientManager()->currentUsername(),
         country               = wizard_->syntheticCountry().toStdString(),
         partyCount            = wizard_->syntheticPartyCount(),
         partyMaxDepth         = wizard_->syntheticPartyMaxDepth(),
         counterpartyCount     = wizard_->syntheticCounterpartyCount(),
         counterpartyMaxDepth  = wizard_->syntheticCounterpartyMaxDepth(),
         portfolioLeafCount    = wizard_->syntheticPortfolioLeafCount(),
         portfolioMaxDepth     = wizard_->syntheticPortfolioMaxDepth(),
         booksPerPortfolio     = wizard_->syntheticBooksPerPortfolio(),
         businessUnitCount     = wizard_->syntheticBusinessUnitCount(),
         businessUnitMaxDepth  = wizard_->syntheticBusinessUnitMaxDepth(),
         generateAddresses     = wizard_->syntheticGenerateAddresses(),
         contactsPerParty      = wizard_->syntheticContactsPerParty(),
         contactsPerCp         = wizard_->syntheticContactsPerCounterparty(),
         generateIdentifiers   = wizard_->syntheticGenerateIdentifiers(),
         seed                  = wizard_->syntheticSeed()]()
            -> PublishResult {

            PublishResult result;

            synthetic::messaging::generate_organisation_request request;
            request.country               = country;
            request.party_count           =
                static_cast<std::uint32_t>(partyCount);
            request.party_max_depth       =
                static_cast<std::uint32_t>(partyMaxDepth);
            request.counterparty_count    =
                static_cast<std::uint32_t>(counterpartyCount);
            request.counterparty_max_depth =
                static_cast<std::uint32_t>(counterpartyMaxDepth);
            request.portfolio_leaf_count  =
                static_cast<std::uint32_t>(portfolioLeafCount);
            request.portfolio_max_depth   =
                static_cast<std::uint32_t>(portfolioMaxDepth);
            request.books_per_leaf_portfolio =
                static_cast<std::uint32_t>(booksPerPortfolio);
            request.business_unit_count   =
                static_cast<std::uint32_t>(businessUnitCount);
            request.business_unit_max_depth =
                static_cast<std::uint32_t>(businessUnitMaxDepth);
            request.generate_addresses    = generateAddresses;
            request.contacts_per_party    =
                static_cast<std::uint32_t>(contactsPerParty);
            request.contacts_per_counterparty =
                static_cast<std::uint32_t>(contactsPerCp);
            request.generate_identifiers  = generateIdentifiers;
            request.seed                  = seed;

            auto genResult = clientManager->process_authenticated_request(
                std::move(request), std::chrono::minutes(10));

            if (!genResult) {
                result.error_message = "Failed to communicate with synthetic service";
                return result;
            }
            if (!genResult->success) {
                result.error_message = genResult->error_message;
                return result;
            }

            result.parties_generated = genResult->parties_count;

            // Associate tenant admin with all Operational parties
            const auto accountId = clientManager->accountId();
            if (!accountId) {
                result.success = true;
                return result;
            }

            refdata::messaging::get_parties_request partyReq;
            partyReq.offset = 0;
            partyReq.limit = 1000;
            auto partyResult = clientManager->process_authenticated_request(
                std::move(partyReq));

            if (!partyResult) {
                result.success = true;
                return result;
            }

            iam::messaging::save_account_party_request apReq;
            for (const auto& party : partyResult->parties) {
                if (party.party_category != "Operational")
                    continue;
                iam::domain::account_party ap;
                ap.account_id = *accountId;
                ap.party_id = party.id;
                ap.tenant_id = party.tenant_id;
                ap.modified_by = publishedBy;
                ap.performed_by = publishedBy;
                ap.change_reason_code =
                    std::string(reason::codes::new_record);
                ap.change_commentary =
                    "Tenant provisioning: tenant admin associated with party";
                apReq.account_parties.push_back(std::move(ap));
            }

            if (!apReq.account_parties.empty()) {
                auto apResult = clientManager->process_authenticated_request(
                    std::move(apReq));
                if (apResult && apResult->success) {
                    result.parties_linked =
                        static_cast<int>(apReq.account_parties.size());
                }
            }

            result.success = true;
            return result;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("Generating synthetic parties, counterparties, and "
                  "organisational structure..."));
}

// ============================================================================
// TenantApplyAndSummaryPage
// ============================================================================

TenantApplyAndSummaryPage::TenantApplyAndSummaryPage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void TenantApplyAndSummaryPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* headerLabel = new QLabel(
        tr("Tenant setup complete"), this);
    headerLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    headerLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(headerLabel);

    layout->addSpacing(10);

    summaryLabel_ = new QLabel(this);
    summaryLabel_->setWordWrap(true);
    summaryLabel_->setTextFormat(Qt::RichText);
    summaryLabel_->setAlignment(Qt::AlignLeft | Qt::AlignTop);
    layout->addWidget(summaryLabel_);

    layout->addStretch();

    auto* nextStepsBox = new QGroupBox(tr("Next Steps"), this);
    auto* nextStepsLayout = new QVBoxLayout(nextStepsBox);
    auto* nextStepsLabel = new QLabel(
        tr("Log out and log back in, then select a party to complete its "
           "operational setup. The party setup wizard will appear automatically "
           "for each party you log into."),
        this);
    nextStepsLabel->setWordWrap(true);
    nextStepsLabel->setTextFormat(Qt::RichText);
    nextStepsLayout->addWidget(nextStepsLabel);
    layout->addWidget(nextStepsBox);
}

void TenantApplyAndSummaryPage::initializePage() {
    // Clear the bootstrap flag
    wizard_->clearBootstrapFlag();

    // Build summary
    QString summary = tr("<p>Your tenant has been set up successfully.</p>");

    if (!wizard_->selectedBundleCode().isEmpty()) {
        summary += tr("<p><b>Reference data bundle:</b> %1</p>")
            .arg(wizard_->selectedBundleName());
    }

    const int partiesLinked = wizard_->partiesLinkedCount();
    if (partiesLinked > 0) {
        summary += tr("<p><b>Parties created and linked:</b> %1 "
                      "(your account has been associated with all of them)</p>")
            .arg(partiesLinked);
    }

    summary += tr("<p>The bootstrap mode flag has been cleared. "
                  "Log out and select a party on next login to complete "
                  "per-party setup.</p>");

    summaryLabel_->setText(summary);

    emit wizard_->provisioningCompleted();
}

}
