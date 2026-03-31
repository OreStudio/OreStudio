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
#include "ores.qt/PartyProvisioningWizard.hpp"
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
#include <QPushButton>
#include <QRegularExpressionValidator>
#include <QSizePolicy>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.synthetic.api/messaging/generate_organisation_protocol.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace reason = ores::database::domain::change_reason_constants;

// ============================================================================
// PartyProvisioningWizard
// ============================================================================

PartyProvisioningWizard::PartyProvisioningWizard(
    ClientManager* clientManager,
    QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager) {

    setWindowTitle(tr("Party Setup"));
    setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::BuildingSkyscraper, IconUtils::DefaultIconColor));
    setMinimumSize(900, 700);
    resize(900, 700);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();

}

void PartyProvisioningWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Welcome, new PartyWelcomePage(this));
    setPage(Page_DataSourceSelection, new PartyDataSourceSelectionPage(this));
    setPage(Page_PartySetup, new PartySetupPage(this));
    setPage(Page_CounterpartySetup, new PartyCounterpartySetupPage(this));
    setPage(Page_OrganisationSetup, new PartyOrganisationSetupPage(this));
    setPage(Page_ReportSetup, new PartyReportSetupPage(this));
    setPage(Page_ReportInstall, new PartyReportInstallPage(this));
    setPage(Page_Summary, new PartyApplyAndSummaryPage(this));

    setStartId(Page_Welcome);
}

bool PartyProvisioningWizard::markPartyActive() {
    BOOST_LOG_SEV(lg(), info) << "Setting current party status to Active";

    const auto party_id = clientManager_->currentPartyId();

    // Fetch the current party record so we can do a full save with status=Active.
    // A limit of 1000 is sufficient here: a freshly provisioned tenant will only
    // ever have the single party that was created in the TenantProvisioningWizard.
    refdata::messaging::get_parties_request list_req;
    list_req.offset = 0;
    list_req.limit = 1000;
    auto list_result = clientManager_->process_authenticated_request(std::move(list_req));
    if (!list_result) {
        BOOST_LOG_SEV(lg(), warn) << "markPartyActive: failed to fetch parties: "
                                  << list_result.error();
        return false;
    }

    refdata::domain::party party;
    bool found = false;
    for (const auto& p : list_result->parties) {
        if (p.id == party_id) {
            party = p;
            found = true;
            break;
        }
    }
    if (!found) {
        BOOST_LOG_SEV(lg(), warn) << "markPartyActive: party not found in list";
        return false;
    }

    party.status = "Active";
    party.change_commentary = "Party setup wizard completed";

    refdata::messaging::save_party_request save_req;
    save_req.data = std::move(party);
    auto save_result = clientManager_->process_authenticated_request(std::move(save_req));
    if (!save_result) {
        BOOST_LOG_SEV(lg(), warn) << "markPartyActive: failed to save party: "
                                  << save_result.error();
        return false;
    }
    if (!save_result->success) {
        BOOST_LOG_SEV(lg(), warn) << "markPartyActive: save_party failed: "
                                  << (save_result->message.empty() ? "Unknown error" : save_result->message);
        return false;
    }
    BOOST_LOG_SEV(lg(), info) << "Party status set to Active successfully";
    return true;
}

// ============================================================================
// PartyWelcomePage
// ============================================================================

PartyWelcomePage::PartyWelcomePage(PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Welcome"));
    setupUI();
}

void PartyWelcomePage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* welcomeLabel = new QLabel(
        tr("Welcome to the party setup wizard."), this);
    welcomeLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    welcomeLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(welcomeLabel);

    layout->addSpacing(10);

    auto* descLabel = new QLabel(this);
    descLabel->setWordWrap(true);
    descLabel->setText(
        tr("This wizard will help you set up the operational structure of "
           "your tenant, including parties, counterparties, organisational "
           "hierarchy, and report definitions.\n\n"
           "The setup process includes:"));
    layout->addWidget(descLabel);

    auto* stepsLabel = new QLabel(this);
    stepsLabel->setWordWrap(true);
    stepsLabel->setTextFormat(Qt::RichText);
    stepsLabel->setText(
        tr("<ol>"
           "<li><b>Choose Data Source</b> - Select between GLEIF registry "
           "or generated synthetic data for parties, counterparties, and "
           "organisational structure.</li>"
           "<li><b>Organisation Setup</b> - Populate your organisation with "
           "the selected data source.</li>"
           "<li><b>Report Definitions</b> - Optionally create a set of "
           "standard risk report definitions.</li>"
           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    auto* noteBox = new QGroupBox(tr("Note"), this);
    auto* noteLayout = new QVBoxLayout(noteBox);
    auto* noteLabel = new QLabel(
        tr("You can skip this setup by clicking Cancel. You can set up "
           "parties and reports manually using the Parties window and "
           "Reporting menu."),
        this);
    noteLabel->setWordWrap(true);
    noteLayout->addWidget(noteLabel);
    layout->addWidget(noteBox);
}

// ============================================================================
// PartyDataSourceSelectionPage
// ============================================================================

PartyDataSourceSelectionPage::PartyDataSourceSelectionPage(
    PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Choose Data Source"));
    setSubTitle(tr("Select how to populate parties, counterparties, and "
                   "organisational structure for your tenant."));

    setupUI();
}

void PartyDataSourceSelectionPage::setupUI() {
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
            this, &PartyDataSourceSelectionPage::onModeChanged);
    connect(syntheticRadio_, &QRadioButton::toggled,
            this, &PartyDataSourceSelectionPage::onModeChanged);

    WidgetUtils::setupComboBoxes(this);
}

void PartyDataSourceSelectionPage::onModeChanged() {
    syntheticOptions_->setVisible(syntheticRadio_->isChecked());
}

bool PartyDataSourceSelectionPage::validatePage() {
    if (syntheticRadio_->isChecked()) {
        wizard_->setDataSourceMode(
            PartyProvisioningWizard::DataSourceMode::synthetic);
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
            PartyProvisioningWizard::DataSourceMode::gleif);
    }
    return true;
}

int PartyDataSourceSelectionPage::nextId() const {
    if (syntheticRadio_->isChecked()) {
        // Skip LEI/counterparty pages, go straight to organisation setup
        return PartyProvisioningWizard::Page_OrganisationSetup;
    }
    // GLEIF flow: go to party setup
    return PartyProvisioningWizard::Page_PartySetup;
}

// ============================================================================
// PartySetupPage
// ============================================================================

PartySetupPage::PartySetupPage(PartyProvisioningWizard* wizard)
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
// PartyCounterpartySetupPage
// ============================================================================

PartyCounterpartySetupPage::PartyCounterpartySetupPage(
    PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Counterparty Import"));
    setupUI();
}

void PartyCounterpartySetupPage::setupUI() {
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
// PartyOrganisationSetupPage
// ============================================================================

PartyOrganisationSetupPage::PartyOrganisationSetupPage(
    PartyProvisioningWizard* wizard)
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

bool PartyOrganisationSetupPage::isComplete() const {
    return publishComplete_;
}

void PartyOrganisationSetupPage::initializePage() {
    publishComplete_ = false;
    publishSuccess_ = false;
    logOutput_->clear();
    progressBar_->setRange(0, 0);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");

    const bool isSynthetic = wizard_->dataSourceMode() ==
        PartyProvisioningWizard::DataSourceMode::synthetic;

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

void PartyOrganisationSetupPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void PartyOrganisationSetupPage::startPublish() {
    const bool isSynthetic = wizard_->dataSourceMode() ==
        PartyProvisioningWizard::DataSourceMode::synthetic;

    if (isSynthetic) {
        startSyntheticGeneration();
    } else {
        startBundlePublish();
    }
}

void PartyOrganisationSetupPage::startBundlePublish() {
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

    // Run both publishes sequentially on a background thread
    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, publishedBy, leiParamsJson,
         hasLei, rootLei]() -> std::optional<ResponseType> {

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

void PartyOrganisationSetupPage::startSyntheticGeneration() {
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Generating synthetic organisation data";

    using ResponseType = synthetic::messaging::generate_organisation_response;

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
// PartyReportSetupPage
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
         "Capital Valuation Adjustment (KVA) for regulatory capital "
         "cost allocation. Primary output for XVA desk P&L, regulatory "
         "reporting, and counterparty credit risk management.",
     .schedule = "0 8 * * 1-5"},
    {.name = "Dynamic Initial Margin",
     .description =
         "Dynamic Initial Margin (DIM) projection over the simulation "
         "horizon using regression-based models (ISDA SIMM or "
         "parametric VaR). Used to compute Margin Valuation Adjustment "
         "(MVA) — the funding cost of posting variation margin and "
         "initial margin over the life of a portfolio.",
     .schedule = "0 8 * * 1-5"},
    {.name = "Wrong-Way Risk",
     .description =
         "Identifies and quantifies wrong-way risk (WWR): the adverse "
         "correlation between counterparty credit quality and exposure. "
         "Flags trades where default probability is positively correlated "
         "with exposure, requiring capital add-ons or counterparty "
         "limit adjustments under Basel III/IV.",
     .schedule = "0 9 * * 1-5"},
    // --- Scenario analysis (9-10 am) -------------------------------------
    {.name = "Stress Test",
     .description =
         "P&L and exposure impact under predefined stress scenarios "
         "(rate shocks, FX crises, credit spread widening, equity "
         "crashes). Outputs per-scenario P&L impact, VaR breach "
         "probability, and limit utilisation. Required for ICAAP, "
         "supervisory stress tests, and internal risk appetite "
         "monitoring.",
     .schedule = "0 9 * * 1-5"},
    {.name = "Scenario Analysis",
     .description =
         "What-if analysis under custom user-defined market scenarios. "
         "Reprices the entire portfolio under each scenario to show "
         "P&L sensitivity to specific market moves. Used for trading "
         "strategy evaluation, hedging optimisation, and regulatory "
         "scenario submissions.",
     .schedule = "0 9 * * 1-5"},
    {.name = "Historical Simulation VaR",
     .description =
         "Value at Risk computed by replaying historical market moves "
         "against today's portfolio. Uses at least 1 year (250 trading "
         "days) of market data, as required by Basel III/IV. Outputs "
         "1-day and 10-day VaR at 99% confidence for regulatory capital "
         "and internal risk limit monitoring.",
     .schedule = "0 9 * * 1-5"},
    {.name = "Parametric VaR",
     .description =
         "Delta-normal parametric Value at Risk using the correlation "
         "matrix and sensitivity vector. Faster than Monte Carlo or "
         "historical simulation; used for real-time intraday limit "
         "monitoring and for benchmarking full revaluation VaR results.",
     .schedule = "0 9 * * 1-5"},
    {.name = "Expected Shortfall",
     .description =
         "Expected Shortfall (ES, also called CVaR) — the expected "
         "loss conditional on the loss exceeding the VaR threshold. "
         "Basel IV (FRTB) replaces VaR with ES at 97.5% confidence as "
         "the primary market risk capital metric. Computed by averaging "
         "the tail losses beyond the 97.5th percentile.",
     .schedule = "0 9 * * 1-5"},
    // --- Regulatory capital (10-11 am) -----------------------------------
    {.name = "FRTB IMA",
     .description =
         "Fundamental Review of the Trading Book (FRTB) Internal "
         "Models Approach capital charge. Computes Expected Shortfall "
         "under the IMA framework, including non-modellable risk factors "
         "(NMRF) stress scenarios and P&L attribution testing (PLAT). "
         "Requires regulatory approval. Higher precision but significant "
         "operational cost.",
     .schedule = "0 10 * * 1-5"},
    {.name = "FRTB SA",
     .description =
         "Fundamental Review of the Trading Book (FRTB) Standardised "
         "Approach capital charge. Computes the sensitivity-based method "
         "(SBM) capital requirement using supervisory prescribed delta, "
         "vega, and curvature sensitivities across all risk classes. "
         "Floor model and fallback for desks not approved for IMA.",
     .schedule = "0 10 * * 1-5"},
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

PartyReportSetupPage::PartyReportSetupPage(PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Report Definitions"));
    setSubTitle(tr("Optionally create a set of standard risk report definitions "
                   "for your party. You can add, modify, or remove these later "
                   "from the Reporting menu."));

    setupUI();
}

void PartyReportSetupPage::setupUI() {
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

bool PartyReportSetupPage::validatePage() {
    std::vector<PartyProvisioningWizard::ReportSpec> selected;
    for (int i = 0; i < reportList_->count(); ++i) {
        const auto* item = reportList_->item(i);
        if (item->checkState() == Qt::Checked) {
            PartyProvisioningWizard::ReportSpec spec;
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

int PartyReportSetupPage::nextId() const {
    int checked = 0;
    for (int i = 0; i < reportList_->count(); ++i) {
        if (reportList_->item(i)->checkState() == Qt::Checked) {
            ++checked;
        }
    }
    return checked > 0
        ? PartyProvisioningWizard::Page_ReportInstall
        : PartyProvisioningWizard::Page_Summary;
}

// ============================================================================
// PartyReportInstallPage
// ============================================================================

PartyReportInstallPage::PartyReportInstallPage(PartyProvisioningWizard* wizard)
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

bool PartyReportInstallPage::isComplete() const {
    return installComplete_;
}

void PartyReportInstallPage::initializePage() {
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

void PartyReportInstallPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void PartyReportInstallPage::startInstall() {
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
                    "Created during party provisioning";

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
// PartyApplyAndSummaryPage
// ============================================================================

PartyApplyAndSummaryPage::PartyApplyAndSummaryPage(
    PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Party Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void PartyApplyAndSummaryPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* headerLabel = new QLabel(
        tr("Party setup complete"), this);
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

void PartyApplyAndSummaryPage::initializePage() {
    // Set the current party status to Active now that setup is complete.
    const bool activated = wizard_->markPartyActive();

    // Build summary
    QString summary;
    if (!activated) {
        summary = tr("<p><b>Warning:</b> Could not activate the party — "
                     "the party setup wizard may reappear on your next login. "
                     "Please contact your administrator.</p>");
    } else {
        summary = tr("<p>Your party setup has been completed successfully.</p>");
    }

    if (!wizard_->rootLei().isEmpty()) {
        summary += tr("<p><b>Root party (LEI):</b> %1 (%2)</p>")
            .arg(wizard_->rootLeiName(), wizard_->rootLei());
    }

    if (wizard_->organisationPublished()) {
        if (wizard_->dataSourceMode() ==
            PartyProvisioningWizard::DataSourceMode::synthetic) {
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

    summary += tr("<p>The party setup flag has been cleared. This wizard "
                  "will not appear on your next login.</p>");

    summaryLabel_->setText(summary);

    emit wizard_->provisioningCompleted();
}

}
