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
#include "ores.qt/OreImportWizard.hpp"

#include <set>
#include <QDate>
#include <QPalette>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QFileDialog>
#include <QFutureWatcher>
#include <QSizePolicy>
#include <QtConcurrent>
#include "ores.ore/scanner/ore_directory_scanner.hpp"
#include "ores.ore/planner/ore_import_planner.hpp"
#include "ores.ore/hierarchy/ore_hierarchy_builder.hpp"
#include "ores.refdata/messaging/currency_protocol.hpp"
#include "ores.refdata/messaging/portfolio_protocol.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.trading/messaging/trade_protocol.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WidgetUtils.hpp"

namespace {

struct ImportResult {
    bool success = false;
    QString error;
    int currencies = 0;
    int portfolios = 0;
    int books = 0;
    int trades = 0;
};

}

namespace ores::qt {

using namespace ores::logging;

// ============================================================================
// OreImportWizard
// ============================================================================

OreImportWizard::OreImportWizard(ClientManager* clientManager, QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager) {

    setWindowTitle(tr("Import ORE Data"));
    setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::BriefcaseFilled, IconUtils::DefaultIconColor));
    setMinimumSize(800, 600);
    resize(800, 600);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    // Default choices
    choices_.scan_exclusions  = {};          // scan all directories by default
    choices_.hierarchy_strip  = {"Input"};   // strip "Input" from hierarchy paths
    choices_.create_parent_portfolio = true;
    choices_.currency_mode = ore::planner::currency_import_mode::missing_only;
    choices_.party_id = clientManager_->currentPartyId();

    setupPages();
}

void OreImportWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Welcome,     new OreWelcomePage(this));
    setPage(Page_Directory,   new OreDirectoryPage(this));
    setPage(Page_ScanSummary, new OreScanSummaryPage(this));
    setPage(Page_Currency,    new OreCurrencyPage(this));
    setPage(Page_Portfolio,   new OrePortfolioPage(this));
    setPage(Page_TradeImport, new OreTradeImportPage(this));
    setPage(Page_Done,        new OreDonePage(this));
    setStartId(Page_Welcome);
}

// ============================================================================
// OreWelcomePage
// ============================================================================

OreWelcomePage::OreWelcomePage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Welcome to the ORE Import Wizard"));
    setSubTitle(tr("This wizard imports currencies, portfolios, books, and "
                   "trades from an ORE example directory into OreStudio."));

    auto* layout = new QVBoxLayout(this);

    auto* label = new QLabel(
        tr("<p>The wizard will guide you through the following steps:</p>"
           "<ol>"
           "<li><b>Directory</b> — select the root ORE directory to import</li>"
           "<li><b>Scan Summary</b> — review detected files and configure exclusions</li>"
           "<li><b>Currencies</b> — choose whether to import all or only missing currencies</li>"
           "<li><b>Portfolio Hierarchy</b> — set an optional parent portfolio name</li>"
           "<li><b>Trade Import</b> — set trade defaults and execute the import</li>"
           "<li><b>Done</b> — review results</li>"
           "</ol>"
           "<p>Click <b>Next</b> to begin.</p>"),
        this);
    label->setWordWrap(true);
    layout->addWidget(label);
    layout->addStretch();
}

// ============================================================================
// OreDirectoryPage
// ============================================================================

OreDirectoryPage::OreDirectoryPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Select ORE Directory"));
    setSubTitle(tr("Choose the root directory containing the ORE data files to import."));

    auto* layout = new QVBoxLayout(this);

    auto* row = new QHBoxLayout;
    dirEdit_ = new QLineEdit(this);
    dirEdit_->setPlaceholderText(tr("Path to ORE directory..."));
    browseBtn_ = new QPushButton(tr("Browse…"), this);
    row->addWidget(dirEdit_);
    row->addWidget(browseBtn_);
    layout->addLayout(row);

    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);  // indeterminate
    progressBar_->hide();
    layout->addWidget(progressBar_);

    layout->addStretch();

    connect(browseBtn_, &QPushButton::clicked, this, &OreDirectoryPage::onBrowseClicked);
    connect(dirEdit_, &QLineEdit::textChanged, this, [this](const QString&) {
        scanComplete_ = false;
        statusLabel_->clear();
        emit completeChanged();
    });
}

bool OreDirectoryPage::isComplete() const {
    // While scanning: keep Next disabled so the user can't re-trigger
    if (scanning_) return false;
    // After a successful scan: allow advancing
    if (scanComplete_) return true;
    // Path entered but not yet scanned: enable Next so the user can click it to start the scan
    return !dirEdit_->text().trimmed().isEmpty();
}

void OreDirectoryPage::onBrowseClicked() {
    const auto dir = QFileDialog::getExistingDirectory(
        this, tr("Select ORE Directory"), dirEdit_->text());
    if (!dir.isEmpty())
        dirEdit_->setText(dir);
}

bool OreDirectoryPage::validatePage() {
    if (scanComplete_)
        return true;
    if (!scanning_)
        startScan();
    return false;  // stay on page; completeChanged() re-enables Next when done
}

void OreDirectoryPage::startScan() {
    const auto path = dirEdit_->text().trimmed();
    if (path.isEmpty()) {
        statusLabel_->setText(tr("Please select a directory."));
        return;
    }

    scanning_ = true;
    emit completeChanged();  // disable Next while scan runs

    statusLabel_->setText(tr("Scanning…"));
    progressBar_->show();
    browseBtn_->setEnabled(false);
    dirEdit_->setEnabled(false);

    const std::filesystem::path root = path.toStdString();
    const auto scan_exclusions = wizard_->choices().scan_exclusions;

    auto* watcher = new QFutureWatcher<ore::scanner::scan_result>(this);
    connect(watcher, &QFutureWatcher<ore::scanner::scan_result>::finished,
            this, &OreDirectoryPage::onScanFinished);

    watcher->setFuture(QtConcurrent::run([root, scan_exclusions]() {
        ore::scanner::ore_directory_scanner scanner(root, scan_exclusions);
        return scanner.scan();
    }));
}

void OreDirectoryPage::onScanFinished() {
    auto* watcher =
        static_cast<QFutureWatcher<ore::scanner::scan_result>*>(sender());
    if (!watcher) return;

    const auto result = watcher->result();
    watcher->deleteLater();

    scanning_ = false;
    progressBar_->hide();
    browseBtn_->setEnabled(true);
    dirEdit_->setEnabled(true);

    wizard_->setScanResult(result);

    const int currencies = static_cast<int>(result.currency_files.size());
    const int portfolios = static_cast<int>(result.portfolio_files.size());

    statusLabel_->setText(
        tr("Scan complete: %1 currency file(s), %2 portfolio file(s).")
        .arg(currencies).arg(portfolios));

    BOOST_LOG_SEV(lg(), info) << "Scan complete: " << currencies
                              << " currency, " << portfolios << " portfolio files";

    scanComplete_ = true;
    emit completeChanged();
}

// ============================================================================
// OreScanSummaryPage
// ============================================================================

OreScanSummaryPage::OreScanSummaryPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Scan Summary"));
    setSubTitle(tr("Review the detected files and configure directory exclusions."));

    auto* layout = new QVBoxLayout(this);

    summaryLabel_ = new QLabel(this);
    summaryLabel_->setWordWrap(true);
    layout->addWidget(summaryLabel_);

    auto* excGroup = new QGroupBox(tr("Excluded directories"), this);
    auto* excLayout = new QVBoxLayout(excGroup);

    exclusionList_ = new QListWidget(this);
    excLayout->addWidget(exclusionList_);

    auto* addRow = new QHBoxLayout;
    exclusionEdit_ = new QLineEdit(this);
    exclusionEdit_->setPlaceholderText(tr("Directory name to exclude…"));
    addBtn_    = new QPushButton(tr("Add"),    this);
    removeBtn_ = new QPushButton(tr("Remove"), this);
    addRow->addWidget(exclusionEdit_);
    addRow->addWidget(addBtn_);
    addRow->addWidget(removeBtn_);
    excLayout->addLayout(addRow);
    layout->addWidget(excGroup);

    hierarchyLabel_ = new QLabel(this);
    hierarchyLabel_->setWordWrap(true);
    layout->addWidget(hierarchyLabel_);

    connect(addBtn_,    &QPushButton::clicked, this, &OreScanSummaryPage::onExclusionAdded);
    connect(removeBtn_, &QPushButton::clicked, this, &OreScanSummaryPage::onExclusionRemoved);
}

void OreScanSummaryPage::initializePage() {
    // Populate exclusion list from current choices
    exclusionList_->clear();
    for (const auto& e : wizard_->choices().scan_exclusions)
        exclusionList_->addItem(QString::fromStdString(e));

    refreshSummary();
}

void OreScanSummaryPage::onExclusionAdded() {
    const auto text = exclusionEdit_->text().trimmed();
    if (text.isEmpty()) return;

    auto& excl = wizard_->choices().scan_exclusions;
    const auto s = text.toStdString();
    if (std::find(excl.begin(), excl.end(), s) == excl.end()) {
        excl.push_back(s);
        exclusionList_->addItem(text);
    }
    exclusionEdit_->clear();
    refreshSummary();
}

void OreScanSummaryPage::onExclusionRemoved() {
    const auto* item = exclusionList_->currentItem();
    if (!item) return;
    const auto s = item->text().toStdString();
    auto& excl = wizard_->choices().scan_exclusions;
    excl.erase(std::remove(excl.begin(), excl.end(), s), excl.end());
    delete exclusionList_->takeItem(exclusionList_->row(item));
    refreshSummary();
}

void OreScanSummaryPage::refreshSummary() {
    const auto& sr = wizard_->scanResult();
    summaryLabel_->setText(
        tr("Found <b>%1</b> currency file(s) and <b>%2</b> portfolio file(s) "
           "(%3 file(s) ignored).")
        .arg(sr.currency_files.size())
        .arg(sr.portfolio_files.size())
        .arg(sr.ignored_files.size()));

    // Rebuild hierarchy to show portfolio/book node count
    ore::hierarchy::ore_hierarchy_builder builder(
        sr.portfolio_files, sr.root, wizard_->choices().hierarchy_strip);
    const auto nodes = builder.build();

    long portfolios = std::count_if(nodes.begin(), nodes.end(),
        [](const auto& n) {
            return n.type == ore::hierarchy::import_node::node_type::portfolio;
        });
    long books = static_cast<long>(nodes.size()) - portfolios;

    hierarchyLabel_->setText(
        tr("Hierarchy preview: <b>%1</b> portfolio(s), <b>%2</b> book(s).")
        .arg(portfolios).arg(books));
}

// ============================================================================
// OreCurrencyPage
// ============================================================================

OreCurrencyPage::OreCurrencyPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Currency Import"));
    setSubTitle(tr("Choose which currencies to import."));

    auto* layout = new QVBoxLayout(this);

    allRadio_ = new QRadioButton(
        tr("Import all currencies from the ORE file"), this);
    missingRadio_ = new QRadioButton(
        tr("Import only currencies not already present in OreStudio"), this);
    missingRadio_->setChecked(true);

    layout->addWidget(allRadio_);
    layout->addWidget(missingRadio_);

    statusLabel_ = new QLabel(tr("Fetching existing currencies…"), this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);

    countLabel_ = new QLabel(this);
    layout->addWidget(countLabel_);

    layout->addStretch();

    connect(allRadio_,     &QRadioButton::toggled, this, &OreCurrencyPage::onModeChanged);
    connect(missingRadio_, &QRadioButton::toggled, this, &OreCurrencyPage::onModeChanged);
}

void OreCurrencyPage::initializePage() {
    fetchDone_ = false;
    statusLabel_->setText(tr("Fetching existing currencies from server…"));
    countLabel_->clear();

    auto* cm = wizard_->clientManager();

    // Fetch all existing ISO codes asynchronously
    using Result = std::set<std::string>;
    auto* watcher = new QFutureWatcher<Result>(this);
    connect(watcher, &QFutureWatcher<Result>::finished,
            this, &OreCurrencyPage::onFetchFinished);

    watcher->setFuture(QtConcurrent::run([cm]() -> Result {
        refdata::messaging::get_currencies_request req;
        req.limit = 1000;  // server enforces max 1000 per request
        const auto resp = cm->process_authenticated_request(std::move(req));
        Result codes;
        if (resp) {
            for (const auto& c : resp->currencies)
                codes.insert(c.iso_code);
        }
        return codes;
    }));
}

void OreCurrencyPage::onFetchFinished() {
    auto* watcher = static_cast<QFutureWatcher<std::set<std::string>>*>(sender());
    if (!watcher) return;

    wizard_->setExistingIsoCodes(watcher->result());
    watcher->deleteLater();

    const int existing = static_cast<int>(wizard_->existingIsoCodes().size());
    statusLabel_->setText(tr("Server has %1 existing currencies.").arg(existing));

    fetchDone_ = true;
    onModeChanged();
}

void OreCurrencyPage::onModeChanged() {
    using mode = ore::planner::currency_import_mode;
    wizard_->choices().currency_mode =
        missingRadio_->isChecked() ? mode::missing_only : mode::all;

    if (!fetchDone_) return;

    // Compute how many will be imported with current mode
    const auto& sr = wizard_->scanResult();
    const auto& existing = wizard_->existingIsoCodes();
    const auto& choices = wizard_->choices();

    ore::planner::ore_import_planner planner(sr, existing, choices);
    const auto plan = planner.plan();

    countLabel_->setText(
        tr("This will import %1 currency record(s).")
        .arg(plan.currencies.size()));
}

// ============================================================================
// OrePortfolioPage
// ============================================================================

OrePortfolioPage::OrePortfolioPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Portfolio Hierarchy"));
    setSubTitle(tr("Configure the parent portfolio for imported data."));

    auto* layout = new QVBoxLayout(this);

    createParentCheck_ = new QCheckBox(
        tr("Set a portfolio as the parent of imported portfolios"), this);
    createParentCheck_->setChecked(true);
    layout->addWidget(createParentCheck_);

    auto* nameRow = new QHBoxLayout;
    nameRow->addWidget(new QLabel(tr("Parent portfolio:"), this));
    parentCombo_ = new QComboBox(this);
    parentCombo_->setEditable(true);
    parentCombo_->setPlaceholderText(tr("Select or type a portfolio name…"));
    nameRow->addWidget(parentCombo_);
    layout->addLayout(nameRow);

    hierarchyPreviewLabel_ = new QLabel(this);
    hierarchyPreviewLabel_->setWordWrap(true);
    layout->addWidget(hierarchyPreviewLabel_);

    layout->addStretch();

    connect(createParentCheck_, &QCheckBox::toggled,
            this, &OrePortfolioPage::onCreateParentToggled);
}

void OrePortfolioPage::initializePage() {
    fetchDone_ = false;
    createParentCheck_->setChecked(wizard_->choices().create_parent_portfolio);

    // Fetch existing portfolios asynchronously to populate combo
    parentCombo_->clear();
    parentCombo_->setEnabled(false);

    auto* cm = wizard_->clientManager();
    using Result = std::vector<std::string>;
    auto* watcher = new QFutureWatcher<Result>(this);
    connect(watcher, &QFutureWatcher<Result>::finished,
            this, &OrePortfolioPage::onPortfoliosFetchFinished);

    watcher->setFuture(QtConcurrent::run([cm]() -> Result {
        refdata::messaging::get_portfolios_request req;
        req.limit = 1000;
        const auto resp = cm->process_authenticated_request(std::move(req));
        Result names;
        if (resp) {
            for (const auto& p : resp->portfolios)
                names.push_back(p.name);
            std::sort(names.begin(), names.end());
        }
        return names;
    }));

    onCreateParentToggled(createParentCheck_->isChecked());
}

bool OrePortfolioPage::validatePage() {
    wizard_->choices().create_parent_portfolio = createParentCheck_->isChecked();
    wizard_->choices().parent_portfolio_name =
        parentCombo_->currentText().trimmed().toStdString();
    return true;
}

void OrePortfolioPage::onPortfoliosFetchFinished() {
    auto* watcher = static_cast<QFutureWatcher<std::vector<std::string>>*>(sender());
    if (!watcher) return;

    const auto names = watcher->result();
    watcher->deleteLater();

    BOOST_LOG_SEV(lg(), info) << "Fetched " << names.size() << " existing portfolios";

    wizard_->setExistingPortfolioNames(names);

    parentCombo_->clear();
    for (const auto& n : names)
        parentCombo_->addItem(QString::fromStdString(n));

    // Restore previously chosen name if any
    const auto prev = QString::fromStdString(wizard_->choices().parent_portfolio_name);
    if (!prev.isEmpty())
        parentCombo_->setCurrentText(prev);

    parentCombo_->setEnabled(createParentCheck_->isChecked());
    fetchDone_ = true;
}

void OrePortfolioPage::onCreateParentToggled(bool checked) {
    parentCombo_->setEnabled(checked && fetchDone_);

    // Show quick preview counts
    const auto& sr = wizard_->scanResult();
    const auto& choices = wizard_->choices();

    ore::hierarchy::ore_hierarchy_builder builder(
        sr.portfolio_files, sr.root, choices.hierarchy_strip);
    const auto nodes = builder.build();

    long portfolios = std::count_if(nodes.begin(), nodes.end(),
        [](const auto& n) {
            return n.type == ore::hierarchy::import_node::node_type::portfolio;
        });
    long books = static_cast<long>(nodes.size()) - portfolios;

    if (checked) {
        hierarchyPreviewLabel_->setText(
            tr("%1 portfolio(s) + 1 parent + %2 book(s) will be created.")
            .arg(portfolios).arg(books));
    } else {
        hierarchyPreviewLabel_->setText(
            tr("%1 portfolio(s) + %2 book(s) will be created.")
            .arg(portfolios).arg(books));
    }
}

// ============================================================================
// OreTradeImportPage
// ============================================================================

OreTradeImportPage::OreTradeImportPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Trade Import"));
    setSubTitle(tr("Set trade defaults and execute the import."));

    auto* layout = new QVBoxLayout(this);

    auto* form = new QFormLayout;
    tradeDateEdit_ = new QLineEdit(this);
    tradeDateEdit_->setPlaceholderText(tr("YYYY-MM-DD (leave blank to keep ORE value)"));
    form->addRow(tr("Default trade date:"), tradeDateEdit_);

    lifecycleEventEdit_ = new QLineEdit(this);
    lifecycleEventEdit_->setPlaceholderText(tr("e.g. New (leave blank to keep ORE value)"));
    form->addRow(tr("Default lifecycle event:"), lifecycleEventEdit_);
    layout->addLayout(form);

    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 4);
    progressBar_->hide();
    layout->addWidget(progressBar_);

    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->hide();
    layout->addWidget(logOutput_);
}

bool OreTradeImportPage::isComplete() const {
    if (importDone_)    return true;   // allow advancing after import
    if (importStarted_) return false;  // disable Next while running
    return true;                       // enable Next to trigger import
}

void OreTradeImportPage::initializePage() {
    importDone_    = false;
    importStarted_ = false;
    statusLabel_->setText(tr("Click Next to start the import."));
    progressBar_->hide();
    logOutput_->hide();
    logOutput_->clear();

    // Pre-fill defaults: today's date and "New" lifecycle event
    if (tradeDateEdit_->text().isEmpty())
        tradeDateEdit_->setText(QDate::currentDate().toString("yyyy-MM-dd"));
    if (lifecycleEventEdit_->text().isEmpty())
        lifecycleEventEdit_->setText(tr("New"));
}

bool OreTradeImportPage::validatePage() {
    if (importDone_) return true;
    if (!importStarted_) startImport();
    return false;
}

void OreTradeImportPage::appendLog(const QString& msg) {
    logOutput_->append(msg);
    BOOST_LOG_SEV(lg(), info) << msg.toStdString();
}

void OreTradeImportPage::startImport() {
    importStarted_ = true;
    emit completeChanged();  // disable Next while import runs

    // Capture defaults from form
    auto& defs = wizard_->choices().defaults;
    defs.trade_date     = tradeDateEdit_->text().trimmed().toStdString();
    defs.lifecycle_event = lifecycleEventEdit_->text().trimmed().toStdString();

    progressBar_->setValue(0);
    progressBar_->show();
    logOutput_->show();
    statusLabel_->setText(tr("Building import plan…"));

    // Build plan on background thread
    const auto sr       = wizard_->scanResult();
    const auto existing = wizard_->existingIsoCodes();
    const auto choices  = wizard_->choices();

    struct ImportResult {
        bool success;
        QString error;
        int currencies;
        int portfolios;
        int books;
        int trades;
    };

    auto* cm = wizard_->clientManager();

    auto* watcher = new QFutureWatcher<ImportResult>(this);
    connect(watcher, &QFutureWatcher<ImportResult>::finished,
            this, &OreTradeImportPage::onImportFinished);

    watcher->setFuture(QtConcurrent::run([sr, existing, choices, cm]() -> ImportResult {
        auto& local_lg = OreTradeImportPage::lg();

        // Helper: build an error string from a failed expected response.
        // Uses resp->message when available, falls back to transport error text.
        auto make_error = [&](const auto& resp, const char* step) -> QString {
            if (!resp) {
                const auto msg = comms::net::to_string(resp.error());
                BOOST_LOG_SEV(local_lg, error)
                    << step << " transport error: " << msg;
                return QString::fromStdString(std::string(step) + ": " + msg);
            }
            BOOST_LOG_SEV(local_lg, error)
                << step << " server error: " << resp->message;
            return QString::fromStdString(std::string(step) + ": " + resp->message);
        };

        try {
            ore::planner::ore_import_planner planner(sr, existing, choices);
            const auto plan = planner.plan();

            BOOST_LOG_SEV(local_lg, info) << "Import plan: "
                << plan.currencies.size() << " currencies, "
                << plan.portfolios.size() << " portfolios, "
                << plan.books.size() << " books, "
                << plan.trades.size() << " trades";

            ImportResult res;

            // Step 1: currencies (single batch)
            if (!plan.currencies.empty()) {
                auto req = refdata::messaging::save_currency_request::from(
                    plan.currencies);
                const auto resp = cm->process_authenticated_request(std::move(req));
                if (!resp || !resp->success)
                    return {.success=false, .error=make_error(resp, "Currency save")};
                res.currencies = static_cast<int>(plan.currencies.size());
                BOOST_LOG_SEV(local_lg, info) << "Saved " << res.currencies << " currencies";
            }

            // Step 2: portfolios (single batch)
            if (!plan.portfolios.empty()) {
                auto req = refdata::messaging::save_portfolio_request::from(
                    plan.portfolios);
                const auto resp = cm->process_authenticated_request(std::move(req));
                if (!resp || !resp->success)
                    return {.success=false, .error=make_error(resp, "Portfolio save"),
                            .currencies=res.currencies};
                res.portfolios = static_cast<int>(plan.portfolios.size());
                BOOST_LOG_SEV(local_lg, info) << "Saved " << res.portfolios << " portfolios";
            }

            // Step 3: books (single batch)
            if (!plan.books.empty()) {
                auto req = refdata::messaging::save_book_request::from(plan.books);
                const auto resp = cm->process_authenticated_request(std::move(req));
                if (!resp || !resp->success)
                    return {.success=false, .error=make_error(resp, "Book save"),
                            .currencies=res.currencies, .portfolios=res.portfolios};
                res.books = static_cast<int>(plan.books.size());
                BOOST_LOG_SEV(local_lg, info) << "Saved " << res.books << " books";
            }

            // Step 4: trades in batches of trade_batch_size
            constexpr int trade_batch_size = 100;
            const int total_trades = static_cast<int>(plan.trades.size());
            const int num_batches = total_trades == 0 ? 0
                : (total_trades + trade_batch_size - 1) / trade_batch_size;
            BOOST_LOG_SEV(local_lg, info)
                << "Sending " << total_trades << " trades in "
                << num_batches << " batch(es) of up to "
                << trade_batch_size << " each";
            for (int offset = 0; offset < total_trades; offset += trade_batch_size) {
                const int end = std::min(offset + trade_batch_size, total_trades);
                std::vector<trading::domain::trade> batch;
                batch.reserve(static_cast<std::size_t>(end - offset));
                for (int i = offset; i < end; ++i)
                    batch.push_back(plan.trades[static_cast<std::size_t>(i)].trade);

                BOOST_LOG_SEV(local_lg, debug) << "Sending trade batch "
                    << (offset / trade_batch_size + 1) << "/"
                    << num_batches << ": " << batch.size() << " trades";

                auto req = trading::messaging::save_trade_request::from(
                    std::move(batch));
                const auto resp = cm->process_authenticated_request(std::move(req));
                if (!resp || !resp->success)
                    return {.success=false, .error=make_error(resp, "Trade save"),
                            .currencies=res.currencies, .portfolios=res.portfolios,
                            .books=res.books};
                res.trades += end - offset;
            }
            if (total_trades > 0)
                BOOST_LOG_SEV(local_lg, info) << "Saved " << res.trades << " trades";

            res.success = true;
            return res;
        } catch (const std::exception& ex) {
            BOOST_LOG_SEV(local_lg, error) << "Import exception: " << ex.what();
            return {.success=false, .error=QString::fromStdString(ex.what())};
        }
    }));
}

void OreTradeImportPage::onImportFinished() {
    auto* fw = static_cast<QFutureWatcher<ImportResult>*>(sender());
    if (!fw) return;

    const auto res = fw->result();
    fw->deleteLater();

    progressBar_->setValue(4);
    wizard_->setImportResults(res.currencies, res.portfolios, res.books, res.trades);
    wizard_->setImportSuccess(res.success);
    wizard_->setImportError(res.error);

    if (res.success) {
        appendLog(tr("Saved %1 currencies.").arg(res.currencies));
        appendLog(tr("Saved %1 portfolios.").arg(res.portfolios));
        appendLog(tr("Saved %1 books.").arg(res.books));
        appendLog(tr("Saved %1 trades.").arg(res.trades));
        statusLabel_->setText(tr("Import completed successfully."));
        BOOST_LOG_SEV(lg(), info) << "ORE import complete: "
            << res.currencies << " currencies, "
            << res.portfolios << " portfolios, "
            << res.books << " books, "
            << res.trades << " trades";
        importDone_ = true;
        emit completeChanged();
    } else {
        appendLog(tr("Error: %1").arg(res.error));
        statusLabel_->setText(
            tr("Import failed: %1\nClick Cancel to abort the wizard.").arg(res.error));
        BOOST_LOG_SEV(lg(), error) << "ORE import failed: " << res.error.toStdString();

        // Turn progress bar red to signal failure
        QPalette pal = progressBar_->palette();
        pal.setColor(QPalette::Highlight, Qt::red);
        progressBar_->setPalette(pal);
        // importDone_ stays false — user must cancel rather than advance
    }
}

// ============================================================================
// OreDonePage
// ============================================================================

OreDonePage::OreDonePage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Import Complete"));
    setSubTitle(tr("The ORE import has finished."));

    auto* layout = new QVBoxLayout(this);
    summaryLabel_ = new QLabel(this);
    summaryLabel_->setWordWrap(true);
    layout->addWidget(summaryLabel_);
    layout->addStretch();
}

void OreDonePage::initializePage() {
    if (wizard_->importSuccess()) {
        summaryLabel_->setText(
            tr("<p><b>Import succeeded.</b></p>"
               "<ul>"
               "<li>Currencies: %1</li>"
               "<li>Portfolios: %2</li>"
               "<li>Books: %3</li>"
               "<li>Trades: %4</li>"
               "</ul>"
               "<p>The data is now available in the Portfolio Explorer.</p>")
            .arg(wizard_->savedCurrencies())
            .arg(wizard_->savedPortfolios())
            .arg(wizard_->savedBooks())
            .arg(wizard_->savedTrades()));
    } else {
        summaryLabel_->setText(
            tr("<p><b>Import failed.</b></p><p>%1</p>")
            .arg(wizard_->importError().isEmpty()
                 ? tr("Unknown error") : wizard_->importError()));
    }
}

}
