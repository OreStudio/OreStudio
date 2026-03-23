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
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>
#include <QListWidgetItem>
#include <QRegularExpressionValidator>
#include <QSizePolicy>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.synthetic/messaging/generate_organisation_protocol.hpp"
#include "ores.variability/domain/system_setting.hpp"
#include "ores.variability/messaging/system_settings_protocol.hpp"

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
    setPage(Page_Welcome, new ProvisioningWelcomePage(this));
    setPage(Page_BundleSelection, new BundleSelectionPage(this));
    setPage(Page_BundleInstall, new BundleInstallPage(this));
    setPage(Page_DataSourceSelection, new DataSourceSelectionPage(this));
    setPage(Page_PartySetup, new PartySetupPage(this));
    setPage(Page_CounterpartySetup, new CounterpartySetupPage(this));
    setPage(Page_OrganisationSetup, new OrganisationSetupPage(this));
    setPage(Page_ReportSetup, new ReportSetupPage(this));
    setPage(Page_ReportInstall, new ReportInstallPage(this));
    setPage(Page_Summary, new ApplyAndSummaryPage(this));

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
        tr("This wizard will help you set up your tenant with the essential "
           "reference data needed to get started. You can always modify or "
           "extend this data later using the Data Librarian.\n\n"
           "The setup process includes:"));
    layout->addWidget(descLabel);

    auto* stepsLabel = new QLabel(this);
    stepsLabel->setWordWrap(true);
    stepsLabel->setTextFormat(Qt::RichText);
    stepsLabel->setText(
        tr("<ol>"
           "<li><b>Select Catalogue</b> - Choose a pre-configured "
           "set of reference data (currencies, countries, etc.).</li>"
           "<li><b>Publish Catalogue</b> - Publish the selected data to your "
           "tenant.</li>"
           "<li><b>Choose Data Source</b> - Select between GLEIF registry "
           "or generated synthetic data for parties, counterparties, and "
           "organisational structure.</li>"
           "<li><b>Organisation Setup</b> - Populate your organisation with "
           "the selected data source.</li>"
           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    auto* noteBox = new QGroupBox(tr("Note"), this);
    auto* noteLayout = new QVBoxLayout(noteBox);
    auto* noteLabel = new QLabel(
        tr("You can skip this setup by clicking Cancel. The wizard will not "
           "appear again, and you can set up reference data manually using "
           "the Data Librarian."),
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
// DataSourceSelectionPage
// ============================================================================

DataSourceSelectionPage::DataSourceSelectionPage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Choose Data Source"));
    setSubTitle(tr("Select how to populate parties, counterparties, and "
                   "organisational structure for your tenant."));

    setupUI();
}

void DataSourceSelectionPage::setupUI() {
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
    row++;

    syntheticOptions_->setVisible(false);
    layout->addWidget(syntheticOptions_);

    layout->addStretch();

    connect(gleifRadio_, &QRadioButton::toggled,
            this, &DataSourceSelectionPage::onModeChanged);
    connect(syntheticRadio_, &QRadioButton::toggled,
            this, &DataSourceSelectionPage::onModeChanged);

    WidgetUtils::setupComboBoxes(this);
}

void DataSourceSelectionPage::onModeChanged() {
    syntheticOptions_->setVisible(syntheticRadio_->isChecked());
}

bool DataSourceSelectionPage::validatePage() {
    if (syntheticRadio_->isChecked()) {
        wizard_->setDataSourceMode(
            TenantProvisioningWizard::DataSourceMode::synthetic);
        wizard_->setSyntheticCountry(
            countryCombo_->currentData().toString());
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
        wizard_->setSyntheticContactsPerCounterparty(
            contactsPerCounterpartySpin_->value());
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
        wizard_->setDataSourceMode(
            TenantProvisioningWizard::DataSourceMode::gleif);
    }
    return true;
}

int DataSourceSelectionPage::nextId() const {
    if (syntheticRadio_->isChecked()) {
        // Skip LEI/counterparty pages, go straight to organisation setup
        return TenantProvisioningWizard::Page_OrganisationSetup;
    }
    // GLEIF flow: go to party setup
    return TenantProvisioningWizard::Page_PartySetup;
}

// ============================================================================
// PartySetupPage
// ============================================================================

PartySetupPage::PartySetupPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Party Setup (Optional)"));
    setSubTitle(tr("Optionally select a root LEI entity to configure your "
                   "organisation's party hierarchy."));

    setupUI();
}

void PartySetupPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    instructionLabel_ = new QLabel(this);
    instructionLabel_->setWordWrap(true);
    instructionLabel_->setText(
        tr("Search the GLEIF LEI registry for your organisation. The selected "
           "entity will become the root of your party hierarchy.\n\n"
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

void PartySetupPage::initializePage() {
    if (!leiLoaded_) {
        leiPicker_->load();
        leiLoaded_ = true;
    }
}

bool PartySetupPage::validatePage() {
    // Optional page - always allow advancing
    if (leiPicker_->hasSelection()) {
        wizard_->setRootLei(leiPicker_->selectedLei());
        wizard_->setRootLeiName(leiPicker_->selectedName());
    }
    wizard_->setLeiDatasetSize(
        datasetSizeCombo_->currentData().toString());
    return true;
}

// ============================================================================
// CounterpartySetupPage
// ============================================================================

CounterpartySetupPage::CounterpartySetupPage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Counterparty Import"));
    setupUI();
}

void CounterpartySetupPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* infoLabel = new QLabel(this);
    infoLabel->setWordWrap(true);
    infoLabel->setText(
        tr("Counterparties represent the external entities your organisation "
           "trades with or has business relationships with.\n\n"
           "Counterparties will be imported automatically from the GLEIF LEI "
           "registry in the next step, based on the party hierarchy you "
           "selected.\n\n"
           "You can also add counterparties manually from the Counterparties "
           "window after completing this wizard. Click Next to continue."));
    layout->addWidget(infoLabel);

    layout->addStretch();
}

// ============================================================================
// OrganisationSetupPage
// ============================================================================

OrganisationSetupPage::OrganisationSetupPage(
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

bool OrganisationSetupPage::isComplete() const {
    return publishComplete_;
}

void OrganisationSetupPage::initializePage() {
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

void OrganisationSetupPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void OrganisationSetupPage::startPublish() {
    const std::string publishedBy = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();

    const bool isSynthetic = wizard_->dataSourceMode() ==
        TenantProvisioningWizard::DataSourceMode::synthetic;

    if (isSynthetic) {
        startSyntheticGeneration();
    } else {
        startBundlePublish();
    }
}

void OrganisationSetupPage::startBundlePublish() {
    const std::string publishedBy = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();
    const std::string rootLei = wizard_->rootLei().toStdString();
    const std::string bundleCode = wizard_->selectedBundleCode().toStdString();
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
        // Opt in the LEI party and counterparty datasets
        const std::string size = datasetSize.empty() ? "small" : datasetSize;
        leiParams.opted_in_datasets.push_back("gleif.lei_parties." + size);
        leiParams.opted_in_datasets.push_back("gleif.lei_counterparties." + size);
        leiParamsJson = dq::messaging::build_params_json(leiParams);
    }

    using ResponseType = dq::messaging::publish_bundle_response;

    auto* watcher = new QFutureWatcher<std::optional<ResponseType>>(this);
    connect(watcher, &QFutureWatcher<std::optional<ResponseType>>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result || !result->success) {
            publishSuccess_ = false;
            statusLabel_->setText(tr("Publication failed!"));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");

            if (!result) {
                BOOST_LOG_SEV(lg(), error)
                    << "Organisation publication: no server response";
                appendLog(tr("ERROR: Failed to communicate with server."));
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Organisation publication failed: "
                    << result->error_message;
                appendLog(tr("ERROR: %1").arg(
                    QString::fromStdString(result->error_message)));
            }
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Organisation publication succeeded: "
                << result->datasets_succeeded << " datasets";
            statusLabel_->setText(
                tr("Organisation data published successfully!"));
            appendLog(tr("Published %1 datasets (%2 records inserted, %3 updated).")
                .arg(result->datasets_succeeded)
                .arg(result->total_records_inserted)
                .arg(result->total_records_updated));
            publishSuccess_ = true;
            wizard_->setOrganisationPublished(true);
        }

        publishComplete_ = true;
        emit completeChanged();
    });

    // Run both publishes sequentially on a background thread:
    // 1. If LEI selected: re-publish base bundle with opted-in LEI datasets
    // 2. Publish organisation bundle for business units, portfolios, books
    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, publishedBy, bundleCode, leiParamsJson,
         hasLei]() -> std::optional<ResponseType> {

            // Step 1: Publish LEI parties and counterparties from base bundle
            if (hasLei) {
                dq::messaging::publish_bundle_request leiRequest;
                leiRequest.bundle_code = bundleCode;
                leiRequest.mode = dq::domain::publication_mode::upsert;
                leiRequest.published_by = publishedBy;
                leiRequest.atomic = true;
                leiRequest.params_json = leiParamsJson;

                auto leiResult = clientManager->process_authenticated_request(
                    std::move(leiRequest), std::chrono::minutes(5));

                if (!leiResult) {
                    return std::nullopt;
                }
                if (!leiResult->success) {
                    return *leiResult;
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
                return std::nullopt;
            }
            return *orgResult;
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

void OrganisationSetupPage::startSyntheticGeneration() {
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Generating synthetic organisation data";

    using ResponseType = synthetic::messaging::generate_organisation_response;

    // Build request struct upfront to avoid a long capture list.
    synthetic::messaging::generate_organisation_request request;
    request.country               = wizard_->syntheticCountry().toStdString();
    request.party_count           =
        static_cast<std::uint32_t>(wizard_->syntheticPartyCount());
    request.party_max_depth       =
        static_cast<std::uint32_t>(wizard_->syntheticPartyMaxDepth());
    request.counterparty_count    =
        static_cast<std::uint32_t>(wizard_->syntheticCounterpartyCount());
    request.counterparty_max_depth =
        static_cast<std::uint32_t>(wizard_->syntheticCounterpartyMaxDepth());
    request.portfolio_leaf_count  =
        static_cast<std::uint32_t>(wizard_->syntheticPortfolioLeafCount());
    request.portfolio_max_depth   =
        static_cast<std::uint32_t>(wizard_->syntheticPortfolioMaxDepth());
    request.books_per_leaf_portfolio =
        static_cast<std::uint32_t>(wizard_->syntheticBooksPerPortfolio());
    request.business_unit_count   =
        static_cast<std::uint32_t>(wizard_->syntheticBusinessUnitCount());
    request.business_unit_max_depth =
        static_cast<std::uint32_t>(wizard_->syntheticBusinessUnitMaxDepth());
    request.generate_addresses    = wizard_->syntheticGenerateAddresses();
    request.contacts_per_party    =
        static_cast<std::uint32_t>(wizard_->syntheticContactsPerParty());
    request.contacts_per_counterparty =
        static_cast<std::uint32_t>(wizard_->syntheticContactsPerCounterparty());
    request.generate_identifiers  = wizard_->syntheticGenerateIdentifiers();
    request.seed                  = wizard_->syntheticSeed();

    auto* watcher = new QFutureWatcher<std::optional<ResponseType>>(this);
    connect(watcher, &QFutureWatcher<std::optional<ResponseType>>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result || !result->success) {
            publishSuccess_ = false;
            statusLabel_->setText(tr("Generation failed!"));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");

            if (!result) {
                BOOST_LOG_SEV(lg(), error)
                    << "Synthetic generation: no server response";
                appendLog(tr("ERROR: Failed to communicate with server."));
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Synthetic generation failed: "
                    << result->error_message;
                appendLog(tr("ERROR: %1").arg(
                    QString::fromStdString(result->error_message)));
            }
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Synthetic generation succeeded: "
                << result->parties_count << " parties, "
                << result->counterparties_count << " counterparties"
                << " (seed: " << result->seed << ")";
            statusLabel_->setText(
                tr("Synthetic organisation generated successfully!"));
            appendLog(tr("Generated %1 parties, %2 counterparties, "
                         "%3 business unit types, %4 business units, "
                         "%5 portfolios, %6 books, "
                         "%7 contacts, %8 identifiers.")
                .arg(result->parties_count)
                .arg(result->counterparties_count)
                .arg(result->business_unit_types_count)
                .arg(result->business_units_count)
                .arg(result->portfolios_count)
                .arg(result->books_count)
                .arg(result->contacts_count)
                .arg(result->identifiers_count));
            appendLog(tr("Seed: %1 (use this to reproduce the same organisation)")
                .arg(result->seed));
            publishSuccess_ = true;
            wizard_->setOrganisationPublished(true);
        }

        publishComplete_ = true;
        emit completeChanged();
    });

    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, request = std::move(request)]() mutable
            -> std::optional<ResponseType> {

            auto result = clientManager->process_authenticated_request(
                std::move(request));

            if (!result) {
                return std::nullopt;
            }
            return *result;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("Generating synthetic organisation (country: %1, "
                  "parties: %2, counterparties: %3)...")
        .arg(wizard_->syntheticCountry())
        .arg(wizard_->syntheticPartyCount())
        .arg(wizard_->syntheticCounterpartyCount()));
}

// ============================================================================
// ReportSetupPage
// ============================================================================

namespace {

struct ReportEntry {
    const char* name;
    const char* description;
    const char* schedule;
};

// Full ORE analytic coverage for a typical trading desk, ordered by
// natural execution dependency (calibration → curves → valuation →
// market risk → counterparty risk → scenario analysis → regulatory capital).
// All use report_type="risk" and concurrency_policy="skip".
constexpr std::array<ReportEntry, 28> k_default_reports{{
    // --- Market data & calibration (5-6 am) ----------------------------
    {.name = "Model Calibration",
     .description =
         "Calibrates interest rate, FX, and volatility models "
         "(LGM, Hull-White, SABR, Black-Scholes) to live market data. "
         "Outputs calibrated parameters and fit quality metrics (RMSE). "
         "Must run before exposure simulation, XVA, and sensitivity "
         "analytics that depend on calibrated model parameters.",
     .schedule = "0 5 * * 1-5"},
    {.name = "Yield Curves",
     .description =
         "Bootstraps discount and projection yield curves from market "
         "instruments (deposits, FRAs, swaps, OIS, bonds). Outputs the "
         "full term structure of interest rates used by all pricing "
         "engines. Essential prerequisite for NPV, sensitivity, and "
         "Monte Carlo exposure analytics.",
     .schedule = "0 6 * * 1-5"},
    {.name = "Credit Curves",
     .description =
         "Bootstraps credit default swap (CDS) spread curves for "
         "counterparties and reference entities. Outputs survival "
         "probability and hazard rate term structures. Used by CVA, "
         "DVA, CRIF, and SIMM calculations as the credit risk input.",
     .schedule = "0 6 * * 1-5"},
    {.name = "Correlation",
     .description =
         "Computes and outputs the correlation matrix between risk "
         "factors across all asset classes (rates, FX, equity, credit, "
         "commodities). Used as the covariance input for parametric "
         "VaR, SIMM initial margin, and scenario generation.",
     .schedule = "0 6 * * 1-5"},
    {.name = "Scenario Generation",
     .description =
         "Generates Monte Carlo scenario paths across the simulation "
         "date grid using calibrated stochastic models. Produces the "
         "scenario cube consumed downstream by counterparty exposure, "
         "XVA, dynamic initial margin, and historical simulation VaR "
         "analytics.",
     .schedule = "0 6 * * 1-5"},
    // --- Core valuation (6-7 am) ----------------------------------------
    {.name = "Portfolio NPV",
     .description =
         "Daily mark-to-market net present value of the entire portfolio "
         "in base reporting currency. Produces trade-level and "
         "portfolio-level valuations using today's market data. Primary "
         "source for P&L reporting, risk management, and the basis for "
         "regulatory capital calculations.",
     .schedule = "0 6 * * 1-5"},
    {.name = "Cashflow Report",
     .description =
         "Projects and outputs the complete scheduled cash flow profile "
         "for all live trades: fixed and floating coupons, notional "
         "exchanges, and option exercise payoffs, broken down by date, "
         "counterparty, and currency. Used for liquidity planning, "
         "funding cost analysis, and collateral management.",
     .schedule = "0 6 * * 1-5"},
    {.name = "Portfolio Details",
     .description =
         "Detailed breakdown of all live portfolio positions including "
         "trade attributes, notional, maturity, product type, pricing "
         "model, and book or portfolio allocation. Supports portfolio "
         "management reporting, limit monitoring, and regulatory "
         "position reporting.",
     .schedule = "0 6 * * 1-5"},
    {.name = "CRIF",
     .description =
         "Generates the Common Risk Interchange Format (CRIF) "
         "sensitivity file from trade-level sensitivities. CRIF is the "
         "standardised input format required by the ISDA SIMM margin "
         "model. Covers interest rate, FX, equity, credit qualifying, "
         "credit non-qualifying, and commodity risk classes.",
     .schedule = "0 7 * * 1-5"},
    // --- Market risk (7-8 am) --------------------------------------------
    {.name = "Market Risk Sensitivities",
     .description =
         "First and second order sensitivities by risk factor. Delta "
         "measures exposure to parallel rate or price shifts; Gamma "
         "captures convexity; Vega measures exposure to implied "
         "volatility. Outputs par and zero sensitivities with optional "
         "Jacobian transformation for hedge ratio computation. Core "
         "input for VaR, hedging, and limit monitoring.",
     .schedule = "0 7 * * 1-5"},
    {.name = "Sensitivity Stress",
     .description =
         "Sensitivities recomputed under each predefined stress "
         "scenario, showing how the delta and vega profile shifts under "
         "adverse market conditions. Supports stressed limits monitoring "
         "and hedging strategy review under crisis market conditions.",
     .schedule = "0 7 * * 1-5"},
    {.name = "P&L Report",
     .description =
         "Daily profit and loss by book, portfolio, and product type. "
         "Decomposes P&L into new deals, matured deals, cash flows "
         "received, and MTM change from market moves. Provides the "
         "authoritative P&L number for front office, finance, and risk "
         "management sign-off.",
     .schedule = "0 7 * * 1-5"},
    {.name = "P&L Attribution",
     .description =
         "P&L bridge report decomposing the daily MTM change into "
         "contributions from individual risk factors: delta P&L (rate "
         "and price moves), gamma P&L (convexity), theta (time decay), "
         "vega (volatility change), and unexplained residual. Essential "
         "for model validation, controller sign-off, and regulatory "
         "P&L explain under FRTB.",
     .schedule = "0 7 * * 1-5"},
    // --- Counterparty risk (8-9 am) --------------------------------------
    {.name = "Counterparty Exposure",
     .description =
         "Monte Carlo simulation of future counterparty credit exposure "
         "across the portfolio lifetime. Computes Expected Positive "
         "Exposure (EPE) and Expected Negative Exposure (ENE) profiles "
         "per netting set at each simulation date. Prerequisite for "
         "CVA, DVA, FVA, and Dynamic Initial Margin calculations.",
     .schedule = "0 8 * * 1-5"},
    {.name = "Potential Future Exposure",
     .description =
         "Potential Future Exposure (PFE) at specified confidence levels "
         "(typically 95% and 99%) over the simulation horizon. Outputs "
         "peak PFE and PFE profiles by counterparty and netting set. "
         "Used for credit line utilisation monitoring, internal capital "
         "allocation, and regulatory IMM model validation.",
     .schedule = "0 8 * * 1-5"},
    {.name = "CVA/DVA Report",
     .description =
         "Credit Valuation Adjustment (CVA) and Debit Valuation "
         "Adjustment (DVA) calculated from simulated exposure profiles "
         "and bootstrapped credit curves. Includes Funding Valuation "
         "Adjustment (FVA) for uncollateralised portfolios and "
         "Collateral Valuation Adjustment (COLVA) where applicable. "
         "Primary XVA P&L and pricing adjustment report.",
     .schedule = "0 8 * * 1-5"},
    {.name = "XVA Sensitivities",
     .description =
         "First and second order sensitivities of all XVA components "
         "(CVA, DVA, FVA, COLVA) to underlying market risk factors. "
         "Enables XVA hedging strategy construction, XVA desk limits "
         "monitoring, and attribution of XVA P&L to individual market "
         "moves.",
     .schedule = "0 8 * * 1-5"},
    {.name = "XVA Explain",
     .description =
         "XVA P&L attribution decomposing the daily change in "
         "CVA/DVA/FVA into contributions from new deals, matured deals, "
         "passage of time (theta), and market moves per risk factor "
         "class. Supports XVA desk P&L explain, model validation, and "
         "regulatory audit trails.",
     .schedule = "0 8 * * 1-5"},
    // --- Scenario analysis (9-10 am) -------------------------------------
    {.name = "Stress Test",
     .description =
         "Portfolio P&L under a library of predefined stress scenarios, "
         "including historical crises (2008 financial crisis, 2020 "
         "COVID shock, 1997 Asian crisis) and hypothetical shocks "
         "(parallel rate +200bps, equity -30%, credit spreads +500bps, "
         "FX devaluation). Outputs NPV and P&L change by book and "
         "counterparty for each scenario.",
     .schedule = "0 9 * * 1-5"},
    {.name = "XVA Stress",
     .description =
         "XVA components recomputed under predefined market stress "
         "scenarios. Shows how CVA, DVA, and FVA change under adverse "
         "conditions such as counterparty credit spread widening, "
         "funding cost increases, or market volatility spikes. Used for "
         "stressed regulatory capital requirements and XVA risk limits.",
     .schedule = "0 9 * * 1-5"},
    {.name = "Value at Risk",
     .description =
         "Parametric (delta-gamma-normal) Value-at-Risk using the "
         "sensitivity vector and historical covariance matrix. Computes "
         "1-day and 10-day VaR at 95% and 99% confidence levels with "
         "risk class breakdown. Suitable for FRTB standardised approach "
         "capital requirements and internal risk limits monitoring.",
     .schedule = "0 9 * * 1-5"},
    {.name = "Historical Simulation VaR",
     .description =
         "Full revaluation Value-at-Risk using historical market data "
         "scenarios (typically 1-3 years of daily observations). "
         "Captures non-linear payoffs and fat tails better than "
         "parametric VaR. Includes Expected Shortfall (CVaR) at "
         "97.5%. Provides the basis for Internal Models Approach (IMA) "
         "capital models under FRTB.",
     .schedule = "0 9 * * 1-5"},
    // --- Margin (9-10 am) ------------------------------------------------
    {.name = "Dynamic Initial Margin",
     .description =
         "Model-based Dynamic Initial Margin (DIM) calculated from the "
         "simulated exposure cube using regression against market "
         "scenarios. Captures how initial margin requirements evolve "
         "over the netting set lifetime. Feeds directly into Margin "
         "Valuation Adjustment (MVA) within the XVA framework.",
     .schedule = "0 9 * * 1-5"},
    {.name = "SIMM Initial Margin",
     .description =
         "ISDA SIMM (Standard Initial Margin Model) regulatory margin "
         "per netting set. Aggregates CRIF sensitivities across "
         "interest rate, credit, equity, FX, and commodity risk classes "
         "using the prescribed SIMM methodology. Required for "
         "non-cleared derivatives under BCBS/IOSCO Phase 6 margin "
         "rules.",
     .schedule = "0 9 * * 1-5"},
    {.name = "Initial Margin Schedule",
     .description =
         "Schedule-based initial margin using the simplified BCBS/IOSCO "
         "gross notional schedule method. Provides a conservative "
         "regulatory floor for initial margin obligations without "
         "sensitivity computation. Applicable to smaller counterparty "
         "relationships below the SIMM calculation threshold.",
     .schedule = "0 9 * * 1-5"},
    // --- Regulatory capital (10-11 am) -----------------------------------
    {.name = "SA-CVA",
     .description =
         "Standardised CVA (SA-CVA) regulatory capital charge per "
         "Basel IV / CRR3. Aggregates CVA delta and vega sensitivities "
         "across risk classes using supervisory prescribed delta factors "
         "and correlation matrices. Produces the CVA risk capital "
         "requirement for institutions that elect or are required to "
         "use the SA-CVA approach under FRTB.",
     .schedule = "0 10 * * 1-5"},
    {.name = "BA-CVA",
     .description =
         "Basic CVA (BA-CVA) regulatory capital charge, the simplified "
         "alternative to SA-CVA under Basel IV. Computes capital using "
         "supervisory EAD, maturity, and credit risk weights without "
         "full sensitivity computation. Applicable to institutions "
         "below the material CVA portfolio threshold for SA-CVA.",
     .schedule = "0 10 * * 1-5"},
    {.name = "SA-CCR",
     .description =
         "Standardised Approach for Counterparty Credit Risk (SA-CCR) "
         "Exposure-at-Default (EAD) calculation per Basel III/IV. "
         "Applies supervisory delta, maturity factor, and supervisory "
         "factor to each netting set. Required for Risk-Weighted Asset "
         "(RWA) and leverage ratio calculations. Replaces the legacy "
         "Current Exposure Method (CEM).",
     .schedule = "0 10 * * 1-5"},
}};

} // anonymous namespace

ReportSetupPage::ReportSetupPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Report Definitions"));
    setSubTitle(tr("Optionally create a set of standard risk report definitions "
                   "for your tenant. You can add, modify, or remove these later "
                   "from the Reporting menu."));

    setupUI();
}

void ReportSetupPage::setupUI() {
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(8);

    auto* infoLabel = new QLabel(
        tr("Select the report definitions to create. All reports are "
           "scheduled on weekdays and use the 'skip' concurrency policy "
           "(new runs are skipped while a prior run is still in progress)."),
        this);
    infoLabel->setWordWrap(true);
    layout->addWidget(infoLabel);

    layout->addSpacing(6);

    // Select All / Deselect All buttons
    auto* btnLayout = new QHBoxLayout();
    auto* selectAllBtn = new QPushButton(tr("Select All"), this);
    auto* deselectAllBtn = new QPushButton(tr("Deselect All"), this);
    selectAllBtn->setMaximumWidth(120);
    deselectAllBtn->setMaximumWidth(120);
    btnLayout->addWidget(selectAllBtn);
    btnLayout->addWidget(deselectAllBtn);
    btnLayout->addStretch();
    layout->addLayout(btnLayout);

    reportList_ = new QListWidget(this);
    reportList_->setSpacing(2);
    reportList_->setAlternatingRowColors(true);

    for (const auto& entry : k_default_reports) {
        auto* item = new QListWidgetItem(reportList_);
        item->setCheckState(Qt::Checked);

        // Two-line display: name (bold) + description
        item->setText(QString("%1\n%2")
            .arg(QString::fromUtf8(entry.name))
            .arg(QString::fromUtf8(entry.description)));
        item->setData(Qt::UserRole, QString::fromUtf8(entry.name));
        item->setData(Qt::UserRole + 1, QString::fromUtf8(entry.description));
        item->setData(Qt::UserRole + 2, QString::fromUtf8(entry.schedule));
        reportList_->addItem(item);
    }

    layout->addWidget(reportList_);

    connect(selectAllBtn, &QPushButton::clicked, this, [this]() {
        for (int i = 0; i < reportList_->count(); ++i) {
            reportList_->item(i)->setCheckState(Qt::Checked);
        }
    });

    connect(deselectAllBtn, &QPushButton::clicked, this, [this]() {
        for (int i = 0; i < reportList_->count(); ++i) {
            reportList_->item(i)->setCheckState(Qt::Unchecked);
        }
    });
}

bool ReportSetupPage::validatePage() {
    std::vector<TenantProvisioningWizard::ReportSpec> selected;
    for (int i = 0; i < reportList_->count(); ++i) {
        const auto* item = reportList_->item(i);
        if (item->checkState() == Qt::Checked) {
            TenantProvisioningWizard::ReportSpec spec;
            spec.name = item->data(Qt::UserRole).toString().toStdString();
            spec.description = item->data(Qt::UserRole + 1).toString().toStdString();
            spec.schedule_expression = item->data(Qt::UserRole + 2).toString().toStdString();
            spec.report_type = "risk";
            spec.concurrency_policy = "skip";
            selected.push_back(std::move(spec));
        }
    }
    wizard_->setSelectedReports(std::move(selected));
    return true;
}

int ReportSetupPage::nextId() const {
    // Count checked items
    int checked = 0;
    for (int i = 0; i < reportList_->count(); ++i) {
        if (reportList_->item(i)->checkState() == Qt::Checked) {
            ++checked;
        }
    }
    return checked > 0
        ? TenantProvisioningWizard::Page_ReportInstall
        : TenantProvisioningWizard::Page_Summary;
}

// ============================================================================
// ReportInstallPage
// ============================================================================

ReportInstallPage::ReportInstallPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Creating Report Definitions"));
    setFinalPage(false);

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(tr("Starting..."), this);
    statusLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);
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

bool ReportInstallPage::isComplete() const {
    return installComplete_;
}

void ReportInstallPage::initializePage() {
    installComplete_ = false;
    installSuccess_ = false;
    logOutput_->clear();
    statusLabel_->setText(tr("Creating report definitions..."));
    progressBar_->setRange(0, 0);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");

    startInstall();
}

void ReportInstallPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void ReportInstallPage::startInstall() {
    const auto specs = wizard_->selectedReports();
    const std::string username = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Creating " << specs.size()
                              << " report definitions";

    appendLog(tr("Looking up system party..."));

    struct InstallResult {
        bool success = false;
        int created = 0;
        std::string error;
    };

    auto* watcher = new QFutureWatcher<InstallResult>(this);
    connect(watcher, &QFutureWatcher<InstallResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Report install failed: "
                                       << result.error;
            statusLabel_->setText(tr("Failed to create report definitions."));
            appendLog(tr("ERROR: %1").arg(
                QString::fromStdString(result.error)));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");
            installSuccess_ = false;
        } else {
            BOOST_LOG_SEV(lg(), info) << "Created " << result.created
                                      << " report definitions";
            statusLabel_->setText(
                tr("Created %1 report definition(s) successfully.")
                    .arg(result.created));
            installSuccess_ = true;
        }

        installComplete_ = true;
        emit completeChanged();
    });

    QFuture<InstallResult> future = QtConcurrent::run(
        [clientManager, specs, username]() -> InstallResult {
            InstallResult result;

            // Step 1: find system party
            refdata::messaging::get_parties_request partiesReq;
            partiesReq.limit = 10;
            auto partiesRes = clientManager->process_authenticated_request(
                std::move(partiesReq));

            if (!partiesRes || partiesRes->parties.empty()) {
                result.error = "No parties found; cannot assign report definitions.";
                return result;
            }

            // Use the System-category party (root, no parent). Fall back to first.
            boost::uuids::uuid partyId = partiesRes->parties.front().id;
            for (const auto& p : partiesRes->parties) {
                if (p.party_category == "System") {
                    partyId = p.id;
                    break;
                }
            }

            // Step 2: create each selected report definition
            boost::uuids::random_generator gen;
            namespace reason = ores::database::domain::change_reason_constants;

            for (const auto& spec : specs) {
                reporting::domain::report_definition def;
                def.id = gen();
                def.name = spec.name;
                def.description = spec.description;
                def.report_type = spec.report_type;
                def.schedule_expression = spec.schedule_expression;
                def.concurrency_policy = spec.concurrency_policy;
                def.party_id = partyId;
                def.modified_by = username;
                def.performed_by = username;
                def.change_reason_code =
                    std::string(reason::codes::new_record);
                def.change_commentary =
                    "Created during tenant provisioning";

                reporting::messaging::save_report_definition_request req;
                req.definition = std::move(def);

                auto res = clientManager->process_authenticated_request(
                    std::move(req));

                if (!res || !res->success) {
                    result.error = "Failed to create '" + spec.name + "': " +
                        (res ? res->message : "no server response");
                    return result;
                }

                ++result.created;
            }

            result.success = true;
            return result;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("Creating %1 report definition(s)...")
        .arg(static_cast<int>(specs.size())));
}

// ============================================================================
// ApplyAndSummaryPage
// ============================================================================

ApplyAndSummaryPage::ApplyAndSummaryPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void ApplyAndSummaryPage::setupUI() {
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
        tr("You can now use the full application. Some things you might "
           "want to do:\n\n"
           "  - Open the <b>Data Librarian</b> to manage datasets and bundles\n"
           "  - Open <b>Parties</b> to manage your party hierarchy\n"
           "  - Open <b>Counterparties</b> to add trading counterparties\n"
           "  - Open <b>Accounts</b> to create additional user accounts"),
        this);
    nextStepsLabel->setWordWrap(true);
    nextStepsLabel->setTextFormat(Qt::RichText);
    nextStepsLayout->addWidget(nextStepsLabel);
    layout->addWidget(nextStepsBox);
}

void ApplyAndSummaryPage::initializePage() {
    // Clear the bootstrap flag
    wizard_->clearBootstrapFlag();

    // Build summary
    QString summary = tr("<p>Your tenant has been set up successfully.</p>");

    if (!wizard_->selectedBundleCode().isEmpty()) {
        summary += tr("<p><b>Reference data bundle:</b> %1</p>")
            .arg(wizard_->selectedBundleName());
    }

    if (!wizard_->rootLei().isEmpty()) {
        summary += tr("<p><b>Root party (LEI):</b> %1 (%2)</p>")
            .arg(wizard_->rootLeiName(), wizard_->rootLei());
    }

    if (wizard_->organisationPublished()) {
        if (wizard_->dataSourceMode() ==
            TenantProvisioningWizard::DataSourceMode::synthetic) {
            summary += tr("<p><b>Organisation data:</b> Synthetic parties, "
                          "counterparties, business units, portfolios, and "
                          "trading books generated.</p>");
        } else {
            summary += tr("<p><b>Organisation data:</b> Business units, portfolios, "
                          "and trading books published.</p>");
        }
    }

    const auto& reports = wizard_->selectedReports();
    if (!reports.empty()) {
        summary += tr("<p><b>Report definitions created (%1):</b></p><ul>")
            .arg(static_cast<int>(reports.size()));
        for (const auto& r : reports) {
            summary += tr("<li>%1</li>")
                .arg(QString::fromStdString(r.name));
        }
        summary += tr("</ul>");
    }

    summary += tr("<p>The bootstrap mode flag has been cleared. This wizard "
                  "will not appear on your next login.</p>");

    summaryLabel_->setText(summary);

    emit wizard_->provisioningCompleted();
}

}
